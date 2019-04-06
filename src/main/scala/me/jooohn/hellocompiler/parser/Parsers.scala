package me.jooohn.hellocompiler.parser

import cats.FlatMap
import cats.instances.all._
import cats.syntax.all._
import me.jooohn.hellocompiler.{AST, Untyped}

object Parsers {
  import Parser._
  import Token._
  import cats.data.StateT.pure

  private type Expr = AST.Expr[Untyped]

  type TokenParser[A] = Parser[List[Token], A]

  def exact[A](target: A): TokenParser[A] =
    parse { case head :: rest if head == target => (rest, target) }

  val anyIdent: TokenParser[Ident] = parse {
    case (ident @ Ident(_)) :: rest => (rest, ident)
  }

  def anyIdentIf(f: String => Boolean): TokenParser[Ident] = parse {
    case (ident @ Ident(value)) :: rest if f(value) => (rest, ident)
  }

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

  val default: TokenParser[Expr] =
    for {
      e <- expr
      _ <- eof
    } yield e

  lazy val expr: TokenParser[Expr] =
    letExpr.widen[Expr] | app

  lazy val app: TokenParser[Expr] = {
    val precedence = List(
      anyIdentIf(_.headOption.exists(Set('*', '/', '%'))).asAST,
      anyIdentIf(_.headOption.exists(Set('+', '-'))).asAST,
    ) :+ anyIdent.asAST
    precedence.foldLeft(prefixExpr | term)(infixExpr)
  }

  def infixExpr(priorParser: TokenParser[Expr],
                operatorParser: TokenParser[Expr]): TokenParser[Expr] =
    FlatMap[TokenParser].tailRecM[Option[Expr], Expr](None) {
      case None => priorParser map (_.some.asLeft)
      case Some(acc) =>
        val next =
          for {
            op <- operatorParser
            current <- priorParser
          } yield AST.App(AST.App(op, acc), current): Expr
        next.map(_.some.asLeft[Expr]) | pure(acc.asRight)
    }

  lazy val prefixOperators: Set[String] = Set("+", "-", "!", "~")
  lazy val prefixExpr: TokenParser[Expr] =
    for {
      prefix <- anyIdentIf(prefixOperators)
      e <- term
    } yield AST.App(AST.Ident(s"unary_${prefix.value}"), e)

  lazy val term: TokenParser[Expr] =
    parenExpr | intExpr | trueExpr | falseExpr | identExpr

  lazy val identExpr: TokenParser[Expr] =
    anyIdent map (ident => AST.Ident(ident.value))

  lazy val letExpr: TokenParser[Expr] =
    for {
      _ <- let
      ident <- anyIdent
      _ <- equals
      v <- expr
      _ <- in
      e <- expr
    } yield AST.Let(ident.value, v, e)

  lazy val parenExpr: TokenParser[Expr] =
    for {
      _ <- openParen
      e <- expr
      _ <- closeParen
    } yield e

  lazy val intExpr: TokenParser[Expr] =
    anyIntLit map (int => AST.IntLit(int.value))

  lazy val trueExpr: TokenParser[Expr] =
    trueLit map (_ => AST.TrueLit)

  lazy val falseExpr: TokenParser[Expr] =
    falseLit map (_ => AST.FalseLit)

  implicit class IdentTokenParserOps(parser: TokenParser[Ident]) {

    def asAST: TokenParser[Expr] =
      parser map (ident => AST.Ident(ident.value))

  }

}
