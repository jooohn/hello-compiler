package me.jooohn.hellocompiler.parser

import cats.FlatMap
import cats.data.StateT
import cats.instances.all._
import cats.syntax.all._
import me.jooohn.hellocompiler.ErrorOr

object Tokenizers {
  import Parser._
  import StateT.pure
  import Token._

  final type CharParser[A] = Parser[List[Char], A]
  final type SingleTokenizer = CharParser[Token]
  final type Tokenizer = CharParser[List[Token]]

  val F: FlatMap[CharParser] = FlatMap[CharParser]

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
    "true" -> TrueLit,
    "false" -> FalseLit,
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
    for {
      tokens <- (space.repeat >> one).repeat
      e <- space.repeat >> eof
    } yield (tokens :+ e).toList

  implicit class CharOps(char: Char) {

    def isLowerAlpha: Boolean = 'a' <= char && char <= 'z'
    def isUpperAlpha: Boolean = 'A' <= char && char <= 'Z'

  }
}
