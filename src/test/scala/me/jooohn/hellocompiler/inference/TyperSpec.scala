package me.jooohn.hellocompiler.inference

import org.scalatest.{FunSpec, Matchers}

class TyperSpec extends FunSpec with Matchers {
  import me.jooohn.hellocompiler.AST._

  describe("typeOf") {

    it("should inferable") {
      val result = Typer.typeOf(
        App(
          Lam(
            "x",
            Ident("x"),
          ),
          App(
            App(
              App(
                Ident("if"),
                Ident("true"),
              ),
              Ident("zero"),
            ),
            App(
              Ident("succ"),
              Ident("zero"),
            )
          ),
        )
      )
      result should be(Right(Type.int))
    }

    it("should not inferable") {
      Typer.typeOf(Ident("undefined")) should be(Left(TypeError("undefined: undefined")))
    }

  }

}
