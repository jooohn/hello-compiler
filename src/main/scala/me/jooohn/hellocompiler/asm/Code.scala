package me.jooohn.hellocompiler.asm

sealed trait Code
object Code {

  case class Label(value: String) extends Code
  object Label {
    val main: Label = Label("main")
  }

  sealed trait Instruction extends Code
  object Instruction {


    case class Add(reg: Reg, value: Value) extends Instruction
    case class Sub(reg: Reg, value: Value) extends Instruction
    case class Mov(reg: Reg, value: Value) extends Instruction
    case class Call(label: Label) extends Instruction
    case object Ret extends Instruction
  }

  sealed trait Value
  case class IntLit(value: Int) extends Value

  sealed trait Reg extends Value
  object Reg {

    case object RAX extends Reg
    case object RSI extends Reg
    case object RDI extends Reg
    case object RBP extends Reg

  }

}

