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
        parse(
          """
            |  ((lambda (x) (cons x '(b)))
            |    'a)
            |
          """.stripMargin
        )
      ) should be (parse("(a b)"))

      eval(
        parse(
          """
            |  ((lambda (x y) (cons x (cdr y)))
            |    'z
            |    '(a b c))
            |
          """.stripMargin
        )
      ) should be (parse("(z b c)"))
    }
    it("treats parameters as operators in expressions as well as arguments") {
      eval(
        parse(
          """
          |  ((lambda (f) (f '(b c)))
          |    ’(lambda (x) (cons 'a x)))
          |
        """.stripMargin
        )
      ) should be (parse("(a b c)"))
    }
  }
  describe("function parameter replacement") {
    it("replace parameters recursively") {
      replace(parse("(a b (c d (e) f) (g) h)").asInstanceOf[Lisp], Map(
        Atom("a") -> Atom("A"),
        Atom("b") -> Atom("B"),
        Atom("c") -> Atom("C"),
        Atom("d") -> Atom("D"),
        Atom("e") -> Atom("E"),
        Atom("f") -> Atom("F"),
        Atom("g") -> Atom("G"),
        Atom("h") -> Atom("H")
      )) should be (parse("(A B (C D (E) F) (G) H)"))
    }
  }
}