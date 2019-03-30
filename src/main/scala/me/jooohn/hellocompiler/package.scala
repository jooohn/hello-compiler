package me.jooohn

package object hellocompiler {

  type ErrorOr[A] = Either[CompileError, A]

}
