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
        Mov(Reg.RAX, IntLit(5)),
        Add(Reg.RAX, IntLit(20)),
        Sub(Reg.RAX, IntLit(4)),
        Ret,
      )) should be(Right(21))

    }

    it("works with subroutine") {
      Emulator.run(Vector(
        Label("plus"),
        Add(Reg.RSI, Reg.RDI),
        Mov(Reg.RAX, Reg.RSI),
        Ret,
        Label("main"),
        Mov(Reg.RDI, IntLit(3)),
        Mov(Reg.RSI, IntLit(4)),
        Call(Label("plus")),
        Ret,
      )) should be(Right(7))
    }

  }

}
