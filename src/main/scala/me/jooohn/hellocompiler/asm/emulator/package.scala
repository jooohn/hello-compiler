package me.jooohn.hellocompiler.asm

package object emulator {

  case class RuntimeError(message: String)

  type ErrorOr[A] = Either[RuntimeError, A]

}
