package me.jooohn.hellocompiler

package object inference {

  type ErrorOr[A] = Either[TypeError, A]

}
