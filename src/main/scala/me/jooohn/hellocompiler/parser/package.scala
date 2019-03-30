package me.jooohn.hellocompiler

import cats.data.StateT

package object parser {

  type Parser[S, A] = StateT[ErrorOr, S, A]

}
