package me.jooohn.hellocompiler.generator

import me.jooohn.hellocompiler.AST.Expr
import me.jooohn.hellocompiler.asm.Program
import me.jooohn.hellocompiler.{ErrorOr, Typed}

trait Generator {

  def generate(expr: Expr[Typed]): ErrorOr[Program] = ???

}
object Generator
