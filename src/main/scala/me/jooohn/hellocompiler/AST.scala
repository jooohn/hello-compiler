package me.jooohn.hellocompiler

sealed trait AST
sealed trait Exp extends AST
object AST {
  case class Ident(x: String) extends Exp
  case class IntLit(value: Int) extends Exp
  case object TrueLit extends Exp
  case object FalseLit extends Exp
  case class Lam(x: String, exp: Exp) extends Exp
  case class App(f: Exp, e: Exp) extends Exp
  case class Let(x: String, e: Exp, f: Exp) extends Exp
}
