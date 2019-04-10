package me.jooohn.hellocompiler.generator

import me.jooohn.hellocompiler.{ErrorOr, Type}

case class Scope(symbolTable: SymbolTable, parent: Option[Scope]) {

  def register(symbol: Symbol, tpe: Type, attributes: SymbolTableAttributes): ErrorOr[Scope] = ???

  def child: Scope = Scope(symbolTable = SymbolTable.empty, parent = Some(this))

}
