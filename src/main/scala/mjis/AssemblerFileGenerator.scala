package mjis

import java.io._

import firm.{Program, Backend, Util}
import mjis.asm.AMD64Registers._
import mjis.asm._

import System.{lineSeparator => n}

import scala.io.Source
import scala.collection.JavaConversions._

abstract class AssemblerFileGenerator(config: Config) extends Phase[Unit] {
  def writeCode(): Writer
  val stdlib_files = List("System_out_println_64.s", "calloc_64.s", "main_64.s")

  override def getResult() = {
    val fw = writeCode()

    // concatenate our implementation of System_out_println to the assembly code
    stdlib_files.foreach(file => {
      val stdlib = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(file))
      stdlib.foreach(fw.write(_))
    })
    fw.flush()
  }

  val findings = List()

  def dumpResult(writer: BufferedWriter) = {
    Source.fromFile(config.asmOutFile.toFile).foreach(writer.write(_))
  }
}

class MjisAssemblerFileGenerator(input: AsmProgram, config: Config) extends AssemblerFileGenerator(config) {
  private def opToString(op: Operand): String = op match {
    case r: RegisterOperand if Registers.contains(r.regNr) => "%" + Registers(r.regNr).subregs(r.sizeBytes)
    case r: RegisterOperand => s"%REG${r.regNr}{${r.sizeBytes}}"
    case r: AddressOperand =>
      val params = Seq[Option[String]](
        r.base.map(opToString),
        r.offset.map(opToString),
        if (r.scale != 1) Some(r.scale.toString) else None
      ).flatten
      val displacement = if (r.displacement != 0) r.displacement.toString else ""
      s"$displacement(${params.mkString(",")})"
    case b: BasicBlockOperand => s".L${b.basicBlock.nr}"
    case l: LabelOperand => l.name
    case c: ConstOperand => s"$$${c.value}"
    case a: ActivationRecordOperand => s"${a.offset}(%rbp){${a.sizeBytes}}"
  }

  private def instrToString(instr: Instruction): String = {
    val operandsToPrint = instr.operands.zip(instr.operandSpecs).
      filter { case (_, spec) => !spec.contains(OperandSpec.IMPLICIT) }.
      map(_._1)
    val operandsResult = if (operandsToPrint.isEmpty) "" else " " + operandsToPrint.map(opToString).mkString(", ")
    val instrAndOperands = instr.opcode + instr.suffix + operandsResult

    val comment = instr.comment +
      (if (instr.stackPointerDisplacement != 0) s" - stackPointerDisplacement = ${instr.stackPointerDisplacement}" else "")

    if (comment.nonEmpty)
      // Align comments
      instrAndOperands + Seq.fill((30 - instrAndOperands.length) max 0)(" ").mkString("") + " # " + comment
    else
      instrAndOperands
  }

  private def phiToString(phi: Phi): String =
    s"phi[${phi.srcs.map(opToString).mkString(", ")}] -> ${opToString(phi.dest)}"

  override def writeCode() = {
    val fw = new BufferedWriter(new FileWriter(config.asmOutFile.toFile, /* append */ false))
    fw.write(generateCode())
    fw
  }

  def generateCode(): String = {
    val result = new StringBuilder
    def emit(s: String, indent: Boolean = true) = result.append(if (indent && s.nonEmpty) s"\t$s$n" else s"$s$n")

    emit(".text")
    emit(".p2align 4,,15")

    input.functions.foreach(function => {
      emit("")
      emit(s"${function.name}:", indent = false)

      for (block <- function.basicBlocks) {
        emit(
          (block match {
            case function.prologue => "# Prologue"
            case function.epilogue => "# Epilogue"
            case _ if block.nr >= 0 => s".L${block.nr}: # Block ${block.nr}"
            case _ => "# Unnamed block"
          })
          + ", predecessors: " + block.predecessors.flatten.map(_.nr).mkString(", ")
          + ", successors: " + block.successors.map(_.nr).mkString(", "), indent = false)
        if (block.comment.nonEmpty) emit("# " + block.comment)
        block.phis.foreach(phi => emit(phiToString(phi)))
        block.allInstructions.foreach(instr => emit(instrToString(instr)))
      }
    })

    result.toString()
  }
}

class FirmAssemblerFileGenerator(a: Unit, config: Config) extends AssemblerFileGenerator(config) {
  def writeCode(): Writer = {
    Program.getGraphs.foreach(_.check())
    Util.lowerSels()
    Backend.createAssembler(config.asmOutFile.toString, "<input>")
    new BufferedWriter(new FileWriter(config.asmOutFile.toFile, /* append */ true))
  }
}
