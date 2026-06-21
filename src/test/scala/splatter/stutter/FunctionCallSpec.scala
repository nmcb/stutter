package splatter
package stutter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class FunctionCallSpec extends AnyFunSpec:
  
  import Parser._
  
  describe("A function call"):
    it("works as specified in chapter 2 of roots of lisp..."):
      """
        |  ((lambda (x) (cons x '(b)))
        |    'a)
        |
      """.stripMargin.parse.eval should be ("(a b)".parse)

      """
        |  ((lambda (x y) (cons x (cdr y)))
        |    'z
        |    '(a b c))
        |
      """.stripMargin.parse.eval should be ("(z b c)".parse)
    
    it("treats parameters as operators in expressions as well as arguments"):
        """
        |  ((lambda (f) (f '(b c)))
        |    ’(lambda (x) (cons 'a x)))
        |
      """.stripMargin.parse.eval should be ("(a b c)".parse)
    
  
  describe("structural expression replacement"):
    it("replaces expressions recursively, though maintains structural composition"):
      replace("(a b (c d (e) f) (g) h)".parse, Map(
        Atom("a") -> Atom("A"),
        Atom("b") -> Atom("B"),
        Atom("c") -> Atom("C"),
        Atom("d") -> Lisp(Seq(Atom("DA"), Atom("DB"))),
        Atom("e") -> Atom("E"),
        Atom("f") -> Atom("F"),
        Atom("g") -> Lisp(Nil),
        Atom("h") -> Atom("H")
      )) should be ("(A B (C (DA DB) (E) F) (()) H)".parse)
