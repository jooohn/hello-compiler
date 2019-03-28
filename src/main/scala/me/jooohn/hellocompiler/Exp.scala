package me.jooohn.hellocompiler

sealed trait Exp
object Exp {
  case class Var(x: String) extends Exp
  case class Lam(x: String, exp: Exp) extends Exp
  case class App(f: Exp, e: Exp) extends Exp
  case class Let(x: String, e: Exp, f: Exp) extends Exp
}
