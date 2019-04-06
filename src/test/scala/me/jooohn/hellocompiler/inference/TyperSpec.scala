package me.jooohn.hellocompiler.inference

import me.jooohn.hellocompiler.Type
import org.scalatest.{FunSpec, Matchers}

class TyperSpec extends FunSpec with Matchers {
  import me.jooohn.hellocompiler.AST._

  describe("typeOf") {

    it("should inferable") {
      val result = Typer.infer(
        App(
          Lam(
            "x",
            Ident("x"),
          ),
          App(
            App(
              App(
                Ident("if"),
                TrueLit,
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
      println(result)
      result.map(_.tpe) should be(Right(Type.int))
    }

    it("should not inferable") {
      Typer.infer(Ident("undefined")) should be(
        Left(TypeError("undefined: undefined")))
    }

  }

}
