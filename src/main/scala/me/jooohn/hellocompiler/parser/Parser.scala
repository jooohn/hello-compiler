package me.jooohn.hellocompiler.parser

import cats.instances.either._
import cats.syntax.all._
import cats.{Applicative, ApplicativeError, FlatMap}
import cats.data.NonEmptyVector
import me.jooohn.hellocompiler.CompileError

object Parser {

  implicit class ParserOps[S, A](parser: Parser[S, A]) {

    def |(that: Parser[S, A])(
        implicit A: ApplicativeError[Parser[S, ?], CompileError])
      : Parser[S, A] =
      parser.orElse(that)

    def repeat(implicit A: ApplicativeError[Parser[S, ?], CompileError])
      : Parser[S, Vector[A]] =
      FlatMap[Parser[S, ?]].tailRecM(Vector.empty[A]) { as =>
        (parser map (a => (as :+ a).asLeft[Vector[A]])) | Applicative[Parser[S,
                                                                             ?]]
          .pure(as.asRight)
      }

    def many(implicit A: ApplicativeError[Parser[S, ?], CompileError])
      : Parser[S, NonEmptyVector[A]] =
      for {
        c <- parser
        cs <- repeat
      } yield NonEmptyVector(c, cs)

  }

}
