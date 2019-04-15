package me.jooohn.hellocompiler.asm.emulator

import org.scalatest.{FunSpec, Matchers}

class EmulatorSpec extends FunSpec with Matchers {
  import me.jooohn.hellocompiler.asm._
  import Code._
  import Instruction._

  describe("run") {

    it("works for simple case") {

      Emulator.run(Vector(
        Label("main"),
        Push(Reg.RBP),
        Mov(Reg.RBP, Reg.RSP),
        Mov(Reg.RAX, IntLit(5)),
        Add(Reg.RAX, IntLit(20)),
        Sub(Reg.RAX, IntLit(4)),
        Pop(Reg.RBP),
        Ret,
      )) should be(Right(21))

    }

    it("works with subroutine") {
      Emulator.run(Vector(
        Label("plus"),
        Push(Reg.RBP),
        Mov(Reg.RBP, Reg.RSP),
        Add(Reg.RSI, Reg.RDI),
        Mov(Reg.RAX, Reg.RSI),
        Pop(Reg.RBP),
        Ret,
        Label("main"),
        Push(Reg.RBP),
        Mov(Reg.RBP, Reg.RSP),
        Mov(Reg.RDI, IntLit(3)),
        Mov(Reg.RSI, IntLit(4)),
        Call(Label("plus")),
        Pop(Reg.RBP),
        Ret,
      )) should be(Right(7))
    }

  }

}
