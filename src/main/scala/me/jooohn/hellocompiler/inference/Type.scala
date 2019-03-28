package me.jooohn.hellocompiler.inference

sealed trait Type {
  import Type._

  def ->(that: Type): -> = Type.->(this, that)

  def typeVars: List[TypeVar] = this match {
    case tv @ TypeVar(_) => List(tv)
    case t1 -> t2        => t1.typeVars ::: t2.typeVars
    case TypeCons(_, ts) => ts.flatMap(_.typeVars)
  }

}

object Type {

  case class ->(t1: Type, t2: Type) extends Type {

    override def toString: String = s"${t1.toString} -> ${t2.toString}"

  }
  case class TypeVar(id: String) extends Type
  case class TypeCons(k: String, ts: List[Type]) extends Type

  val int: TypeCons = TypeCons("Int", Nil)
  val boolean: TypeCons = TypeCons("Boolean", Nil)

}
