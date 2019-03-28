package me.jooohn.hellocompiler.inference

import cats.syntax.all._
import me.jooohn.hellocompiler.Exp
import me.jooohn.hellocompiler.Exp._

private class Typer { self =>
  import Type._

  type Env = List[(String, TypeScheme)]

  def typeOf(e: Exp): ErrorOr[Type] = predef.typeOf(e)

  val predef: Env = {

    val booleanType = TypeCons("Boolean", Nil)
    val intType = TypeCons("Int", Nil)
    def listType(t: Type) = TypeCons("List", t :: Nil)

    def gen(t: Type): TypeScheme = (Nil: Env).gen(t)
    val a = newTypeVar

    List(
      "true" -> gen(booleanType),
      "false" -> gen(booleanType),
      "if" -> gen(booleanType -> (a -> (a -> a))),
      "zero" -> gen(intType),
      "succ" -> gen(intType -> intType),
      "nil" -> gen(listType(a)),
      "cons" -> gen(a -> (listType(a) -> listType(a))),
      "isEmpty" -> gen(listType(a) -> booleanType),
      "head" -> gen(listType(a) -> a),
      "tail" -> gen(listType(a) -> listType(a)),
      "fix" -> gen((a -> a) -> a)
    )
  }

  var n: Int = 0

  def newTypeVar: TypeVar = {
    n = n + 1
    TypeVar(s"v$n")
  }

  def unify(t: Type, u: Type, sub: Substitute): ErrorOr[Substitute] =
    (sub(t), sub(u)) match {
      case (TypeVar(a), TypeVar(b)) if a == b =>
        Right(sub.extend(TypeVar(a), u))
      case (TypeVar(a), _) if !u.typeVars.exists(_.id == a) =>
        Right(sub.extend(TypeVar(a), u))
      case (_, TypeVar(_)) => unify(u, t, sub)
      case (t1 -> t2, u1 -> u2) =>
        for {
          sub2 <- unify(t2, u2, sub)
          sub3 <- unify(t1, u1, sub2)
        } yield sub3
      case (TypeCons(k1, ts), TypeCons(k2, us)) if k1 == k2 =>
        (ts zip us).foldLeft(sub.asRight[TypeError]) { (s, tu) =>
          s.flatMap(unify(tu._1, tu._2, _))
        }
      case _ => Left(TypeError(s"cannot unify ${sub(t)} with ${sub(u)}"))
    }

  case class TypeScheme(tvs: List[TypeVar], tpe: Type) {

    def typeVars: List[TypeVar] =
      tpe.typeVars diff tvs

    def newInstance: Type =
      tvs.foldLeft(Substitute.empty)(_.extend(_, newTypeVar))(tpe)

  }

  implicit class EnvOps(env: Env) {

    def lookup(x: String): Option[TypeScheme] = env match {
      case Nil            => None
      case (y, t) :: tail => if (x == y) Some(t) else tail.lookup(x)
    }

    def typeVars: List[TypeVar] = env.flatMap(_._2.typeVars)

    def gen(t: Type): TypeScheme = TypeScheme(
      t.typeVars diff typeVars,
      t,
    )

    def tp(e: Exp, t: Type, sub: Substitute): ErrorOr[Substitute] = {
      e match {
        case Var(x) =>
          for {
            scheme <- lookup(x).toRight(TypeError(s"undefined: $x"))
            sub2 <- unify(scheme.newInstance, t, sub)
          } yield sub2

        case Lam(x, exp) =>
          val a, b = newTypeVar
          for {
            sub1 <- unify(t, a -> b, sub)
            sub2 <- ((x, TypeScheme(Nil, a)) :: env).tp(exp, b, sub1)
          } yield sub2

        case App(e1, e2) =>
          val a = newTypeVar
          for {
            sub1 <- tp(e1, a -> t, sub)
            sub2 <- tp(e2, a, sub1)
          } yield sub2

        case Let(x, e1, e2) =>
          val a = newTypeVar
          for {
            sub1 <- tp(e1, a, sub)
            sub2 <- ((x, gen(sub1(a))) :: env).tp(e2, t, sub1)
          } yield sub2
      }
    }

    def typeOf(e: Exp): ErrorOr[Type] = {
      val a = newTypeVar
      tp(e, a, Substitute.empty).map(_(a))
    }

  }

}
object Typer {

  def typeOf(exp: Exp): ErrorOr[Type] = new Typer().typeOf(exp)
}
