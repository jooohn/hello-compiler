package me.jooohn.hellocompiler.parser

import me.jooohn.hellocompiler.CompileError

case class TokenizeError(message: String) extends CompileError(message)
