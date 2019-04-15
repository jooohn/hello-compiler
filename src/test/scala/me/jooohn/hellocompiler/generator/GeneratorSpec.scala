package me.jooohn.hellocompiler.generator

import me.jooohn.hellocompiler.Typed
import me.jooohn.hellocompiler.asm.emulator.Emulator
import org.scalatest.{FunSpec, Matchers}

class GeneratorSpec extends FunSpec with Matchers {
  import me.jooohn.hellocompiler.AST._
  import me.jooohn.hellocompiler.Type._

  describe("integration") {

    describe("generate") {

      def execute(expr: Expr[Typed]): Int = {
        Generator.generate(IntLit(123)) match {
          case Left(e) => throw new Error(e.message)
          case Right(program) =>
            Emulator.run(program) match {
              case Left(e) => throw new Error(e.message)
              case Right(exitCode) => exitCode
            }
        }
      }

      it("generates asm for just an value") {
        execute(IntLit(123)) should be(123)
      }

      it("generates asm for simple apply") {
        execute {
          App(
            App(
              Ident("+", Typed(int -> int -> int)),
              IntLit(1),
              Typed(int -> int),
            ),
            IntLit(2),
            Typed(int),
          )
        } should be(123)
      }

    }

  }

}
