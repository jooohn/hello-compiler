package me.jooohn.hellocompiler.inference

import me.jooohn.hellocompiler.{Type, Typed}
import org.scalatest.{FunSpec, Matchers}

class TyperSpec extends FunSpec with Matchers {
  import me.jooohn.hellocompiler.AST._

  describe("typeOf") {

    it("should inferable") {
      val result = Typer.infer(
        Let(
          "identity",
          Lam(
            "x",
            Ident("x"),
          ),
          App(
            Ident("identity"),
            App(
              App(
                App(
                  Ident("if"),
                  TrueLit,
                ),
                App(
                  Ident("identity"),
                  Ident("zero"),
                ),
              ),
              IntLit(123),
            ),
          )
        )
      )
      printExpr(result.right.get)
      result.map(_.tpe) should be(Right(Type.int))
    }

    it("should not inferable") {
      Typer.infer(Ident("undefined")) should be(
        Left(TypeError("undefined: undefined")))
    }

  }

  def printExpr(expr: Expr[Typed]): Unit = {
    def go(inner: Expr[Typed], indent: Int): Unit = inner match {
      case IntLit(value) => println("  " * indent + value.toString)
      case TrueLit => println("  " * indent + "TRUE")
      case FalseLit => println("  " * indent + "FALSE")
      case Lam(x, ex, Typed(tpe)) =>
        println("  " * indent + s"${x}: ${tpe} =>")
        go(ex, indent + 1)
      case App(f, e, Typed(tpe)) =>
        println("  " * indent + s"app: ${tpe}")
        go(f, indent + 1)
        go(e, indent + 1)
      case Let(bind, e, f, Typed(tpe)) =>
        println("  " * indent + s"let ${bind}: ${e.tpe} =")
        go(e, indent + 1)
        println("  " * indent + s"in: $tpe")
        go(f, indent + 1)
      case Ident(name, Typed(tpe)) =>
        println("  " * indent + s"($name: $tpe)")
    }
    go(expr, 0)
  }

}
