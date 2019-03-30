package me.jooohn.hellocompiler.parser

import cats.FlatMap
import cats.data.{NonEmptyVector, StateT}
import cats.instances.all._
import cats.syntax.all._
import me.jooohn.hellocompiler.ErrorOr

object Tokenizers {
  import StateT.pure
  import Token._

  final type CharParser[A] = Parser[List[Char], A]
  final type SingleTokenizer = CharParser[Token]
  final type Tokenizer = CharParser[List[Token]]

  val F: FlatMap[CharParser] = FlatMap[CharParser]

  def parse[A](f: PartialFunction[List[Char], (List[Char], A)]): CharParser[A] =
    StateT[ErrorOr, List[Char], A] { chars =>
      if (f.isDefinedAt(chars)) f(chars).asRight
      else
        TokenizeError(
          s"Failed to parse ${chars.take(10)}${if (chars.length > 10) "..."
          else ""}").asLeft
    }

  def char(f: Char => Boolean): CharParser[Char] =
    parse { case c :: rest if f(c) => (rest, c) }

  val space: CharParser[Unit] = char(_.isWhitespace) >> pure(())

  val digit: CharParser[Char] = char(_.isDigit)
  val intLit: SingleTokenizer =
    digit.many map (ds => IntLit(ds.toVector.mkString.toInt))

  val lowerAlpha: CharParser[Char] = char(_.isLowerAlpha)
  val upperAlpha: CharParser[Char] = char(_.isUpperAlpha)
  val alpha: CharParser[Char] = lowerAlpha | upperAlpha
  val alphaNum: CharParser[Char] = digit | alpha

  val identSymbols: Set[Char] = Set(
    '=', '+', '-', '*', '/', '%', ':', '&', '|', '<', '>',
  )

  val reservedIdentifiers: Map[String, Token] = Map(
    "let" -> Let,
    "in" -> In,
    "true" -> True,
    "false" -> False,
    "=" -> Equals,
    ":" -> Colon,
  )

  val identifier: SingleTokenizer = {
    val alphaNumIdent: CharParser[String] =
      for {
        head <- alpha
        tail <- alphaNum.repeat
      } yield (head +: tail).mkString
    val symbolIdent: CharParser[String] =
      char(identSymbols).many.map(_.toVector.mkString)
    (alphaNumIdent | symbolIdent) map { ident =>
      reservedIdentifiers.getOrElse(ident, Ident(ident))
    }
  }

  val openParen: SingleTokenizer = char(_ === '(') >> pure(OpenParen)

  val closeParen: SingleTokenizer = char(_ === ')') >> pure(CloseParen)

  val eof: SingleTokenizer = parse { case Nil => (Nil, EOF) }

  val one: SingleTokenizer =
    intLit | identifier | openParen | closeParen

  val default: Tokenizer =
    F.tailRecM(Vector.empty[Token]) { as =>
      space.repeat >> (
        one.map(t => (as :+ t).asLeft[Vector[Token]]) |
          eof.map(e => (as :+ e).asRight)
      )
    } map (_.toList)

  implicit class ParserOps[A](parser: CharParser[A]) {

    def |(that: CharParser[A]): CharParser[A] = parser.orElse(that)

    def repeat: CharParser[Vector[A]] =
      F.tailRecM(Vector.empty[A]) { as =>
        (parser map (a => (as :+ a).asLeft[Vector[A]])) | pure(as.asRight)
      }

    def many: CharParser[NonEmptyVector[A]] =
      for {
        c <- parser
        cs <- repeat
      } yield NonEmptyVector(c, cs)

  }

  implicit class CharOps(char: Char) {

    def isLowerAlpha: Boolean = 'a' <= char && char <= 'z'
    def isUpperAlpha: Boolean = 'A' <= char && char <= 'Z'

  }
}
