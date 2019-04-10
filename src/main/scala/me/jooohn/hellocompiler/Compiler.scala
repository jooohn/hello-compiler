package me.jooohn.hellocompiler

import cats.instances.all._
import me.jooohn.hellocompiler.AST.Expr
import me.jooohn.hellocompiler.inference.Typer
import me.jooohn.hellocompiler.parser.{Parsers, Token, Tokenizers}

class Compiler {

  def tokenize(code: String): ErrorOr[List[Token]] =
    Tokenizers.default.run(code.toList).map(_._2)

  def parse(code: String): ErrorOr[Expr[Untyped]] =
    for {
      tokens <- tokenize(code)
      expr <- Parsers.default.run(tokens).map(_._2)
    } yield expr

  def typeInference(code: String): ErrorOr[Expr[Typed]] =
    for {
      expr <- parse(code)
      typed <- Typer.infer(expr)
    } yield typed

//  def eval(code: String): ErrorOr[Unit] =
//    for {
//      typed <- typeInference(code)
//      result <- new Evaluator().evaluate(typed)
//    } yield println(result)

}
