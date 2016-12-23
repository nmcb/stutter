package splatter
package stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ParserSpec extends FunSpec {
  describe("Parser") {
    it("should parse atoms") {
      Parser.parse("foo") should be(
        Atom("foo"))
    }
    it("should parse lists") {
      Parser.parse("()") should be(
        Lisp(Nil))
      Parser.parse("(foo)") should be(
        Lisp(Seq(Atom("foo"))))
      Parser.parse("(foo bar)") should be(
        Lisp(Seq(Atom("foo"), Atom("bar"))))
      Parser.parse("(a b (c) d)") should be(
        Lisp(Seq(Atom("a"), Atom("b"), Lisp(Seq(Atom("c"))), Atom("d"))))
    }
    it("should parse quotes") {
      Parser.parse("'a") should be(
        Lisp(Seq(Atom("quote"), Atom("a"))))
      Parser.parse("'(a b c)") should be(
        Lisp(Seq(Atom("quote"), Lisp(Seq(Atom("a"), Atom("b"), Atom("c"))))))
    }
  }
}
