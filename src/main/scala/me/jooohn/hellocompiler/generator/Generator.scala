package me.jooohn.hellocompiler.generator

import cats.syntax.all._
import me.jooohn.hellocompiler.AST.{Expr, IntLit}
import me.jooohn.hellocompiler.asm.Code.Instruction.{Mov, Pop, Push, Ret}
import me.jooohn.hellocompiler.asm.Code.{Label, Reg}
import me.jooohn.hellocompiler.asm.{Code, Program}
import me.jooohn.hellocompiler.{ErrorOr, Typed}

trait Generator {

  def generate(expr: Expr[Typed]): ErrorOr[Program] = {

    def go(current: Expr[Typed]): ErrorOr[Program] =
      current match {
        case IntLit(value) =>
          Vector(
            Push(Code.IntLit(value))
          ).asRight
      }
    go(expr) map { program =>
      Vector(
        Label.main,
        Push(Reg.RBP),
        Mov(Reg.RBP, Reg.RSP)
      ) ++ program ++ Vector(
        Pop(Reg.RAX),
        Pop(Reg.RBP),
        Ret,
      )
    }
  }

}
object Generator extends Generator
