package me.jooohn.hellocompiler.parser

import me.jooohn.hellocompiler.{AST, Exp}
import me.jooohn.hellocompiler.parser.Token.Ident

import scala.reflect.runtime.universe.TypeTag
import cats.instances.all._
import cats.syntax.all._

object Parsers {
  import cats.data.StateT.pure
  import Parser._
  import Token._

  type TokenParser[A] = Parser[List[Token], A]

  def exact[A](target: A): TokenParser[A] =
    parse { case head :: rest if head == target => (rest, target) }

  val anyIdent: TokenParser[Ident] = parse {
    case (ident @ Ident(_)) :: rest => (rest, ident)
  }
  def exactIdent(value: String): TokenParser[Ident] = exact(Ident(value))

  val anyIntLit: TokenParser[IntLit] = parse {
    case (intLit @ IntLit(_)) :: rest => (rest, intLit)
  }

  val let: TokenParser[Let.type] = exact(Let)
  val in: TokenParser[In.type] = exact(In)
  val trueLit: TokenParser[TrueLit.type] = exact(TrueLit)
  val falseLit: TokenParser[FalseLit.type] = exact(FalseLit)

  val equals: TokenParser[Equals.type] = exact(Equals)
  val colon: TokenParser[Colon.type] = exact(Colon)
  val openParen: TokenParser[OpenParen.type] = exact(OpenParen)
  val closeParen: TokenParser[CloseParen.type] = exact(CloseParen)
  val eof: TokenParser[EOF.type] = exact(EOF)

  val default: TokenParser[AST] =
    for {
      e <- exp
      _ <- eof
    } yield e

  lazy val exp: TokenParser[Exp] =
    letExp.widen[Exp] | app

  lazy val app: TokenParser[Exp] = {
    val precedence = List(
      exactIdent("/") | exactIdent("*") | exactIdent("%"),
      exactIdent("+") | exactIdent("-"),
    ) :+ anyIdent
    precedence.foldLeft(term) { (priorParser, operator) =>
      priorParser flatMap { prior =>
        val app: TokenParser[Exp] =
          for {
            op <- operator
            arg <- priorParser
          } yield AST.App(
            AST.App(
              AST.Ident(op.value),
              prior
            ),
            arg,
          )
        app | pure(prior)
      }
    }
  }

  lazy val term: TokenParser[Exp] =
    parenExp |
      intExp.widen[Exp] |
      trueExp.widen[Exp] |
      falseExp.widen[Exp] |
      identExp.widen[Exp]

  lazy val identExp: TokenParser[AST.Ident] =
    anyIdent map (ident => AST.Ident(ident.value))

  lazy val letExp: TokenParser[AST.Let] =
    for {
      _ <- let
      ident <- anyIdent
      _ <- equals
      v <- exp
      _ <- in
      e <- exp
    } yield AST.Let(ident.value, v, e)

  lazy val parenExp: TokenParser[Exp] =
    for {
      _ <- openParen
      e <- exp
      _ <- closeParen
    } yield e

  lazy val intExp: TokenParser[AST.IntLit] =
    anyIntLit map (int => AST.IntLit(int.value))

  lazy val trueExp: TokenParser[AST.TrueLit.type] =
    trueLit map (_ => AST.TrueLit)

  lazy val falseExp: TokenParser[AST.FalseLit.type] =
    falseLit map (_ => AST.FalseLit)

}
