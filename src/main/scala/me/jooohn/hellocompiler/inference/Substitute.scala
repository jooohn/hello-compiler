package me.jooohn.hellocompiler.inference

import me.jooohn.hellocompiler.Type
import me.jooohn.hellocompiler.Type.{TypeCons, TypeVar, ->}

case class Substitute(extended: Map[TypeVar, Type]) {

  def lookup(tv: TypeVar): Type = extended.getOrElse(tv, tv)

  def apply(t: Type): Type = t match {
    case tv @ TypeVar(_) =>
      val u = lookup(tv)
      if (t == u) t else apply(u)

    case t1 -> t2 =>
      apply(t1) -> apply(t2)

    case TypeCons(k, ts) =>
      TypeCons(k, ts map apply)
  }

  def extend(x: TypeVar, t: Type): Substitute =
    copy(extended.updated(x, t))

}
object Substitute {
  val empty: Substitute = Substitute(Map.empty)
}
