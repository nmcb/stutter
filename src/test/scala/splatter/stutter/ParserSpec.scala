package splatter
package stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ParserSpec extends FunSpec {
  import Stutter.Parser._
  describe("Parser") {
    it("should parse atoms") {
      parseLisp("foo") should be(
        Atom("foo"))
    }
    it("should parse lists") {
      parseLisp("()") should be(
        Lisp(Nil))
      parseLisp("(foo)") should be(
        Lisp(Seq(Atom("foo"))))
      parseLisp("(foo bar)") should be(
        Lisp(Seq(Atom("foo"), Atom("bar"))))
      parseLisp("(a b (c) d)") should be(
        Lisp(Seq(Atom("a"), Atom("b"), Lisp(Seq(Atom("c"))), Atom("d"))))
    }
    it("should parse normal single ' quotes on atoms and lists") {
      parseLisp("'a") should be(
        Lisp(Seq(Atom("quote"), Atom("a"))))
      parseLisp("'(a b c)") should be(
        Lisp(Seq(Atom("quote"), Lisp(Seq(Atom("a"), Atom("b"), Atom("c"))))))
    }
    it("should parse ugly, copy-paste, english, burn your fingers, sharp ’ quotes") {
      parseLisp("’a") should be(
        Lisp(Seq(Atom("quote"), Atom("a"))))
    }
    it("should parse complex structures") {
      parseLisp("((lambda (x) (cons x '(b))) 'a)") should be(
        Lisp(Seq(
          Lisp(Seq(
            Atom("lambda"),
            Lisp(Seq(Atom("x"))),
            Lisp(Seq(
              Atom("cons"),
              Atom("x"),
              Lisp(Seq(Atom("quote"),
                Lisp(Seq(
                  Atom("b")
                ))
              ))
            ))
          )),
          Lisp(Seq(Atom("quote"), Atom("a")))
        ))
      )
    }
    it("should handle complex whitespace") {
      parseLisp("(  a  )") should be (parseLisp("(a)"))
      parseLisp(
        """'(
          |  a
          |     b
          |        c
          |)""".stripMargin) should be(parseLisp("'(a b c)"))
    }

    /* Sanity Checks */

    it("should parse lists recursively") {
      parseLisp("(())") should be(Lisp(Seq(Lisp(Nil))))
    }
  }
}
