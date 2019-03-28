package me.jooohn.hellocompiler.inference

import org.scalatest.{FunSpec, Matchers}

class TyperSpec extends FunSpec with Matchers {
  import me.jooohn.hellocompiler.Exp._

  describe("typeOf") {

    it("should inferable") {
      val result = Typer.typeOf(
        App(
          Lam(
            "x",
            Var("x"),
          ),
          App(
            App(
              App(
                Var("if"),
                Var("true"),
              ),
              Var("zero"),
            ),
            App(
              Var("succ"),
              Var("zero"),
            )
          ),
        )
      )
      result should be(Right(Type.int))
    }

    it("should not inferable") {
      Typer.typeOf(Var("undefined")) should be(Left(TypeError("undefined: undefined")))
    }

  }

}
