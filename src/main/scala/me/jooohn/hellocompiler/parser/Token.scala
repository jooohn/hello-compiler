package me.jooohn.hellocompiler.parser

sealed trait Token
object Token {

  case class IntLit(value: Int) extends Token
  case class Ident(value: String) extends Token
  case object TrueLit extends Token
  case object FalseLit extends Token
  case object Let extends Token
  case object In extends Token
  case object Equals extends Token
  case object Colon extends Token
  case object OpenParen extends Token
  case object CloseParen extends Token
  case object EOF extends Token

}
