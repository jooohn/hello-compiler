package me.jooohn.hellocompiler.parser

import cats.instances.either._
import me.jooohn.hellocompiler.parser.Tokenizers.Tokenizer
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers
import org.scalatest.{FunSpec, Matchers}

class TokenizerSpec extends FunSpec with Matchers with Checkers {
  import Token._

  val tokenizer: Tokenizer = Tokenizers.default

  describe("default") {

    it("simple case") {
      val subject =
        """
          | 123 + 456
          |  - abc
          |   """.stripMargin
      tokenizer.run(subject.toList) should be(Right((Nil, List(
        IntLit(123),
        Ident("+"),
        IntLit(456),
        Ident("-"),
        Ident("abc"),
        EOF,
      ))))
    }

    it("complicated case") {
      val subject =
        """
          |let c           = 123 in
          |let lower       = 234 in
          |let Upper       = 567 in
          |let alphaNum999 = 890 in
          |let bool1: Bool = true in
          |let bool2       = false in
          |let bool        = bool1 && bool2 in
          |  (c + lower - Upper * alphaNum999 / 12345) <= 1
          |    || bool
          |
        """.stripMargin
      tokenizer.run(subject.toList) should be(
        Right(
          (Nil,
           List(
             Let :: Ident("c") :: Equals :: IntLit(123) :: In :: Nil,
             Let :: Ident("lower") :: Equals :: IntLit(234) :: In :: Nil,
             Let :: Ident("Upper") :: Equals :: IntLit(567) :: In :: Nil,
             Let :: Ident("alphaNum999") :: Equals :: IntLit(890) :: In :: Nil,
             Let :: Ident("bool1") :: Colon :: Ident("Bool") :: Equals :: True :: In :: Nil,
             Let :: Ident("bool2") :: Equals :: False :: In :: Nil,
             Let :: Ident("bool") :: Equals :: Ident("bool1") :: Ident("&&") :: Ident(
               "bool2") :: In :: Nil,
             OpenParen :: Ident("c") :: Ident("+") :: Ident("lower") :: Ident("-") :: Ident("Upper") :: Ident("*") :: Ident("alphaNum999") :: Ident("/") :: IntLit(12345) :: CloseParen :: Ident("<=") :: IntLit(1) :: Nil,
             Ident("||") :: Ident("bool") :: Nil,
             EOF :: Nil
           ).flatten)))
    }

    it("should not cause stack overflow") {
      def double(exp: String): String = s"($exp + $exp)"
      def repeat(exp: String, times: Int): String =
        if (times <= 0) exp
        else repeat(double(exp), times - 1)
      val subject = repeat("1", 12)
      tokenizer.run(subject.toList).isRight should be(true)
    }

    it("should fail to parse invalid symbol") {
      val invalidSymbol = Gen.oneOf(Seq(
        '#',
        '@',
        '?',
      ))
      val invalidSymbols = Gen.nonEmptyListOf(invalidSymbol)

      forAll(invalidSymbols) { symbols =>
        tokenizer.run(symbols).isLeft
      }
    }

  }

}
