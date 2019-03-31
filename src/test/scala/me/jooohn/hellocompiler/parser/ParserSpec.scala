package me.jooohn.hellocompiler.parser

import cats.instances.either._
import me.jooohn.hellocompiler.AST
import me.jooohn.hellocompiler.parser.Parsers.TokenParser
import org.scalatest.prop.Checkers
import org.scalatest.{FunSpec, Matchers}

class ParserSpec extends FunSpec with Matchers with Checkers {
  import AST._

  val parser: TokenParser[AST] = Parsers.default

  describe("default") {

    it("parses Ident") {
      val tokens = List(Token.Ident("a"), Token.EOF)
      parser.run(tokens) should be(Right((Nil, Ident("a"))))
    }

    it("parses Int literal") {
      val tokens = List(Token.IntLit(12345), Token.EOF)
      parser.run(tokens) should be(Right((Nil, IntLit(12345))))
    }

    it("parses True") {
      val tokens = List(Token.TrueLit, Token.EOF)
      parser.run(tokens) should be(Right((Nil, TrueLit)))
    }

    it("parses False") {
      val tokens = List(Token.FalseLit, Token.EOF)
      parser.run(tokens) should be(Right((Nil, FalseLit)))
    }

    it("parses Let") {

      val tokens = List(Token.Let,
                        Token.Ident("x"),
                        Token.Equals,
                        Token.IntLit(123),
                        Token.In,
                        Token.Ident("x"),
                        Token.EOF)
      parser.run(tokens) should be(
        Right((Nil, Let("x", IntLit(123), Ident("x")))))
    }

    it("parses App") {

      val tokens = List(
        Token.Ident("a"),
        Token.Ident("+"),
        Token.IntLit(10),
        Token.EOF
      )

      parser.run(tokens) should be(
        Right(
          (Nil,
           App(
             App(
               Ident("+"),
               Ident("a"),
             ),
             IntLit(10),
           ))))
    }

    it("parses operators by appropriate order") {
      val tokens = List(
        Token.Ident("a"),
        Token.Ident("*"),
        Token.Ident("b"),
        Token.Ident("+"),
        Token.Ident("c"),
        Token.Ident("/"),
        Token.Ident("d"),
        Token.Ident("-"),
        Token.Ident("e"),
        Token.Ident("%"),
        Token.Ident("f"),
        Token.EOF,
      )
      parser.run(tokens) should be(
        Right(
          (
            Nil,
            App(
              App(
                Ident("-"),
                App(
                  App(
                    Ident("+"),
                    App(
                      App(
                        Ident("*"),
                        Ident("a"),
                      ),
                      Ident("b"),
                    ),
                  ),
                  App(
                    App(
                      Ident("/"),
                      Ident("c"),
                    ),
                    Ident("d"),
                  )
                )
              ),
              App(
                App(
                  Ident("%"),
                  Ident("e"),
                ),
                Ident("f"),
              ),
            ),
          ),
        )
      )
    }

    it("parses prefix") {
      val tokens = List(
        Token.Ident("a"),
        Token.Ident("-"),
        Token.Ident("-"),
        Token.Ident("b"),
        Token.EOF,
      )
      parser.run(tokens) should be(
        Right(
          (
            Nil,
            App(
              App(
                Ident("-"),
                Ident("a"),
              ),
              App(
                Ident("unary_-"),
                Ident("b"),
              )
            )
          )
        )
      )
    }

  }

}
