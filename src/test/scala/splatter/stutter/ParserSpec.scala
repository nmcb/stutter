package splatter
package stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ParserSpec extends FunSpec {
  import Stutter.Parser._
  describe("Parser") {
    it("should parse atoms") {
      parse("foo") should be(
        Atom("foo"))
    }
    it("should parse lists") {
      parse("()") should be(
        Lisp(Nil))
      parse("(foo)") should be(
        Lisp(Seq(Atom("foo"))))
      parse("(foo bar)") should be(
        Lisp(Seq(Atom("foo"), Atom("bar"))))
      parse("(a b (c) d)") should be(
        Lisp(Seq(Atom("a"), Atom("b"), Lisp(Seq(Atom("c"))), Atom("d"))))
    }
    it("should parse normal single ' quotes on atoms and lists") {
      parse("'a") should be(
        Lisp(Seq(Atom("quote"), Atom("a"))))
      parse("'(a b c)") should be(
        Lisp(Seq(Atom("quote"), Lisp(Seq(Atom("a"), Atom("b"), Atom("c"))))))
    }
    it("should parse ugly, copy-paste, english, burn your fingers, sharp ’ quotes") {
      parse("’a") should be(
        Lisp(Seq(Atom("quote"), Atom("a"))))
    }
    it("should parse complex structures") {
      parse("((lambda (x) (cons x '(b))) 'a)") should be(
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
      parse("(  a  )") should be (parse("(a)"))
      parse(
        """'(
          |  a
          |     b
          |        c
          |)""".stripMargin) should be(parse("'(a b c)"))
    }

    /* Sanity Checks */

    it("should parse lists recursively") {
      parse("(())") should be(Lisp(Seq(Lisp(Nil))))
    }
  }
}
