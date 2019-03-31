package me.jooohn.hellocompiler

sealed trait AST
sealed trait Expr extends AST
object AST {
  case class Ident(x: String) extends Expr
  case class IntLit(value: Int) extends Expr
  case object TrueLit extends Expr
  case object FalseLit extends Expr
  case class Lam(x: String, exp: Expr) extends Expr
  case class App(f: Expr, e: Expr) extends Expr
  case class Let(x: String, e: Expr, f: Expr) extends Expr
}
