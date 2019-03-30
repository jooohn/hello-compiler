package me.jooohn.hellocompiler.inference

import me.jooohn.hellocompiler.CompileError

case class TypeError(message: String) extends CompileError(message)
