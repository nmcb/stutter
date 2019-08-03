package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import splatter.stutter.Stutter.FunctionCall.replace

class FunctionCallSpec extends FunSpec {
  import Stutter._
  import Stutter.Parser._
  describe("A function call") {
    it("works as specified in chapter 2 of roots of lisp...") {
      eval(
          """
            |  ((lambda (x) (cons x '(b)))
            |    'a)
            |
          """.stripMargin) should be (parseLisp("(a b)"))

      eval(
          """
            |  ((lambda (x y) (cons x (cdr y)))
            |    'z
            |    '(a b c))
            |
          """.stripMargin) should be (parseLisp("(z b c)"))
    }
    it("treats parameters as operators in expressions as well as arguments") {
      eval(
          """
          |  ((lambda (f) (f '(b c)))
          |    ’(lambda (x) (cons 'a x)))
          |
        """.stripMargin) should be (parseLisp("(a b c)"))
    }
  }
  describe("structural expression replacement") {
    it("replaces expressions recursively, though maintains structural composition") {
      replace(parseLisp("(a b (c d (e) f) (g) h)").asInstanceOf[Lisp], Map(
        Atom("a") -> Atom("A"),
        Atom("b") -> Atom("B"),
        Atom("c") -> Atom("C"),
        Atom("d") -> Lisp(Seq(Atom("DA"), Atom("DB"))),
        Atom("e") -> Atom("E"),
        Atom("f") -> Atom("F"),
        Atom("g") -> Lisp(Nil),
        Atom("h") -> Atom("H")
      )) should be (parseLisp("(A B (C (DA DB) (E) F) (()) H)"))
    }
  }
}
