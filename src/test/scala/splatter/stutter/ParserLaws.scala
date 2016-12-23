package splatter
package stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ParserLaws extends FunSpec {
  describe("Parser") {
    it("should parse expressions") {
      Parser.parse("foo") should be(
        Atom("foo"))
      Parser.parse("()") should be(
        Expr.f)
      Parser.parse("(foo)") should be(
        Lisp(Seq(Atom("foo"))))
      Parser.parse("(foo bar)") should be(
        Lisp(Seq(Atom("foo"), Atom("bar"))))
      Parser.parse("(a b (c) d)") should be(
        Lisp(Seq(Atom("a"), Atom("b"), Lisp(Seq(Atom("c"))), Atom("d"))))
    }
  }
}
