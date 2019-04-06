package me.jooohn.hellocompiler

sealed trait Untyped
object Untyped extends Untyped
case class Typed(tpe: Type) extends Untyped

object AST {
  sealed trait Expr[+T <: Untyped] {

    def typed: T

    def tpe(implicit ev: T <:< Typed): Type = typed.tpe

  }
  case class Lam[T <: Untyped](x: String, exp: Expr[T], typed: T = Untyped) extends Expr[T]
  case class App[T <: Untyped](f: Expr[T], e: Expr[T], typed: T = Untyped) extends Expr[T]
  case class Let[T <: Untyped](bind: String, e: Expr[T], f: Expr[T], typed: T = Untyped) extends Expr[T]

  case class Ident[T <: Untyped](name: String, typed: T = Untyped) extends Expr[T]

  case class IntLit(value: Int) extends Expr[Typed] {
    override def typed: Typed = Typed(Type.int)
  }

  case object TrueLit extends Expr[Typed] {
    override def typed: Typed = Typed(Type.bool)
  }

  case object FalseLit extends Expr[Typed] {
    override def typed: Typed = Typed(Type.bool)
  }

}
