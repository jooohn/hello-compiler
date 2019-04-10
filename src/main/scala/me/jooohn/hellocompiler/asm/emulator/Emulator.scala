package me.jooohn.hellocompiler.asm.emulator

import cats.syntax.all._
import me.jooohn.hellocompiler.asm.{Code, Program}
import me.jooohn.hellocompiler.asm.Code.{Instruction, _}
import Instruction._

object Emulator {

  def run(program: Program): ErrorOr[Int] =
    for {
      hint <- Hint.buildFrom(program)
      exitCode <- Runtime(program, hint).run
    } yield exitCode

}

case class Runtime(program: Program, hint: Hint) {

  def run: ErrorOr[Int] = {
    def go(state: State): ErrorOr[Int] = {
      if (state.isCompleted) state.rax
      else
        (for {
          code <- codeAt(state.pc)
          updated <- code match {
            case instruction: Instruction => execute(state, instruction)
            case _                        => state.asRight
          }
        } yield updated) match {
          case Left(e)  => e.asLeft
          case Right(s) => go(s.step)
        }
    }
    init.flatMap(go)
  }

  def init: ErrorOr[State] =
    hint.posOf(Label.main) map { mainPos =>
      State(
        pc = mainPos,
        stack = Nil,
        register = Register.empty,
        isCompleted = false
      )
    }

  def codeAt(pc: Pos): ErrorOr[Code] =
    program.lift(pc.index).toRight(RuntimeError(s"Invalid pc: $pc"))

  def execute(state: State, instruction: Instruction): ErrorOr[State] =
    instruction match {

      case Mov(dest, src) =>
        state.extractValue(src).map(state.updated(dest))

      case Add(dest, src) =>
        for {
          srcValue <- state.extractValue(src)
          destValue <- state.valueOf(dest)
        } yield state.updated(dest)(destValue + srcValue)

      case Sub(dest, src) =>
        for {
          srcValue <- state.extractValue(src)
          destValue <- state.valueOf(dest)
        } yield state.updated(dest)(destValue - srcValue)

      case Call(label) =>
        for {
          pos <- hint.posOf(label)
        } yield
          state
            .push(state.pc.index)
            .jump(pos)

      // TODO: support subroutine call
      case Ret =>
        Right(state.pop match {
          case (s, Some(value)) => s.jump(Pos(value))
          case (s, None) =>
            s.completed
        })
    }

}

case class Hint(labelPos: Map[Label, Pos]) {

  def memo(label: Label, pos: Pos): ErrorOr[Hint] =
    labelPos.get(label) match {
      case None => copy(labelPos = labelPos.updated(label, pos)).asRight
      case Some(p) =>
        RuntimeError(s"""duplicate label "${label}" at line ${pos} and ${p}""").asLeft
    }

  def posOf(label: Label): ErrorOr[Pos] =
    labelPos
      .get(label)
      .toRight(RuntimeError(s"""label "${label}" does not exist."""))

}
object Hint {

  def empty: Hint = Hint(Map.empty)

  def buildFrom(program: Vector[Code]): ErrorOr[Hint] =
    program.zipWithIndex.foldLeft(Right(empty): ErrorOr[Hint]) {
      case (Right(hint), (label @ Label(_), index)) =>
        hint.memo(label, Pos(index))
      case (acc, _) => acc
    }

}

case class State(
    pc: Pos,
    stack: List[Int],
    register: Register,
    isCompleted: Boolean,
) {

  def step: State = copy(pc = pc.next)

  def push(value: Int): State = copy(stack = value :: stack)

  def pop: (State, Option[Int]) = stack match {
    case head :: tail => (copy(stack = tail), Some(head))
    case _            => (this, None)
  }

  def jump(pos: Pos): State = copy(pc = pos)

  def completed: State = copy(isCompleted = true)

  def extractValue(value: Value): ErrorOr[Int] = value match {
    case IntLit(v) => v.asRight
    case reg: Reg  => valueOf(reg)
  }

  def valueOf(reg: Reg): ErrorOr[Int] =
    register.valueOf(reg).toRight(RuntimeError(s"no value stored in ${reg}"))

  def rax: ErrorOr[Int] = valueOf(Reg.RAX)

  def updated(reg: Reg)(value: Int): State =
    copy(register = register.updated(reg, value))

}

case class Pos(index: Int) extends AnyVal {

  def next: Pos = copy(index + 1)

}

case class Register(valueMap: Map[Reg, Int]) {

  def valueOf(reg: Reg): Option[Int] = valueMap.get(reg)

  def updated(reg: Reg, value: Int): Register =
    copy(valueMap.updated(reg, value))

}
object Register {

  val empty: Register = Register(Map.empty)

}
