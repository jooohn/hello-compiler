package me.jooohn.hellocompiler.generator

import me.jooohn.hellocompiler.{ErrorOr, Type}

sealed trait SymbolTableAttributes

case class SymbolTable(table: Map[Symbol, Map[Type, SymbolTableAttributes]]) {

  def register(symbol: Symbol, tpe: Type, attributes: SymbolTableAttributes): ErrorOr[SymbolTable] = ???

}
object SymbolTable {

  val empty: SymbolTable = SymbolTable(Map.empty)

}
