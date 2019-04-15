package me.jooohn.hellocompiler.parser

import me.jooohn.hellocompiler.CompileError

case class ParseError(message: String) extends CompileError
