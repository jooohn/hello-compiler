package me.jooohn.hellocompiler.inference

import cats.syntax.all._
import me.jooohn.hellocompiler.AST._
import me.jooohn.hellocompiler._

private class Typer { self =>
  import Type._

  type Env = List[(String, TypeScheme)]

  def infer(expr: Expr[Untyped]): ErrorOr[Expr[Typed]] =
    predef.infer(expr)

  val predef: Env = {
    def gen(t: Type): TypeScheme = (Nil: Env).gen(t)
    val a = newTypeVar

    List(
      "if" -> gen(bool -> (a -> (a -> a))),
      "zero" -> gen(int),
      "succ" -> gen(int -> int),
      "nil" -> gen(list(a)),
      "cons" -> gen(a -> (list(a) -> list(a))),
      "isEmpty" -> gen(list(a) -> bool),
      "head" -> gen(list(a) -> a),
      "tail" -> gen(list(a) -> list(a)),
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
        (ts zip us).foldLeft(sub.asRight[CompileError]) { (s, tu) =>
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

    def tp(e: Expr[Typed], sub: Substitute): ErrorOr[Substitute] =
      e match {
        case Ident(x, _) =>
          for {
            scheme <- lookup(x).toRight(TypeError(s"undefined: $x"))
            sub1 <- unify(scheme.newInstance, e.tpe, sub)
          } yield sub1

        case Lam(x, exp, _) =>
          val a = newTypeVar
          for {
            sub1 <- unify(e.tpe, a -> exp.tpe, sub)
            sub2 <- ((x, TypeScheme(Nil, a)) :: env).tp(exp, sub1)
          } yield sub2

        case App(e1, e2, _) =>
          for {
            sub1 <- tp(e1, sub)
            sub2 <- tp(e2, sub1)
            sub3 <- unify(e2.tpe -> e.tpe, e1.tpe, sub2)
          } yield sub3

        case Let(x, e1, e2, _) =>
          for {
            sub1 <- tp(e1, sub)
            sub2 <- unify(e.tpe, e2.tpe, sub1)
            sub3 <- ((x, gen(sub1(e1.tpe))) :: env).tp(e2, sub2)
          } yield sub3

        case _ => sub.asRight
      }

    def infer(expr: Expr[Untyped]): ErrorOr[Expr[Typed]] = {
      val typed = expr.withTypeVar
      tp(typed, Substitute.empty) map { sub =>
        def resolve(expr: Expr[Typed]): Expr[Typed] = expr match {
          case Lam(x, exp, Typed(tpe)) => Lam(x, resolve(exp), Typed(sub(tpe)))
          case App(f, e, Typed(tpe)) =>
            App(resolve(f), resolve(e), Typed(sub(tpe)))
          case Let(bind, e, f, Typed(tpe)) =>
            Let(bind, resolve(e), resolve(f), Typed(sub(tpe)))
          case Ident(name, Typed(tpe)) => Ident(name, Typed(sub(tpe)))
          case lit                     => lit
        }
        resolve(typed)
      }
    }

  }

  implicit class UntypedExprOps(expr: Expr[Untyped]) {

    def withTypeVar: Expr[Typed] = expr match {
      case Ident(x, _)    => Ident(x, Typed(newTypeVar))
      case Lam(x, exp, _) => Lam(x, exp.withTypeVar, Typed(newTypeVar))
      case App(e1, e2, _) =>
        App(e1.withTypeVar, e2.withTypeVar, Typed(newTypeVar))
      case Let(x, e1, e2, _) =>
        Let(x, e1.withTypeVar, e2.withTypeVar, Typed(newTypeVar))
      case lit @ IntLit(_) => lit
      case lit @ TrueLit   => lit
      case lit @ FalseLit  => lit
    }

  }

}
object Typer {

  def infer(expr: Expr[Untyped]): ErrorOr[Expr[Typed]] =
    new Typer().infer(expr)

}
