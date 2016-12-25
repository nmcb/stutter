package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ExpressionAxioms extends FunSpec {
  describe("Expression axioms") {
    import Stutter._
    import Stutter.Parser._
    it("quote expressions should yield the quoted expression") {
      eval(parse("(quote a)")).toString       should be ("a")
      eval(parse("'a")).toString              should be ("a")
      eval(parse("(quote (a b c))")).toString should be ("(a b c)")
    }
    it("atom expressions should yield the atom `t` if the argument yields an atom or the empty list") {
      eval(parse("(atom 'a)")).toString        should be ("t")
      eval(parse("(atom '())")).toString       should be ("t")
      eval(parse("(atom (atom 'a))")).toString should be ("t")
    }
    it("atom expression should yield the empty list if the argument does not yield an atom or the empty list") {
      eval(parse("(atom '(atom 'a))")).toString should be ("()")
      eval(parse("(atom '(a b c))")).toString   should be ("()")
    }
    it("eq expression should yield the atom `t` if both arguments yield the same atom or both the empty list") {
      eval(parse("(eq 'a 'a)")).toString   should be ("t")
      eval(parse("(eq '() '())")).toString should be ("t")
    }
    it("eq expression should yield the empty list if both arguments do not yield the same atom or the empty list") {
      eval(parse("(eq 'a 'b)")).toString should be ("()")
    }
    it("car expression should yield the first element of its argument list") {
      eval(parse("(car '(a b c))")).toString should be ("a")
    }
    it("cdr expression should yield everything after the first element of its argument list") {
      eval(parse("(cdr '(a b c))")).toString should be ("(b c)")
    }
    it("cons expression should return a list containing the value of its first argument followed by the elements of the value of its second argument") {
      eval(parse("(cons 'a '(b c))")).toString should be ("(a b c)")
      eval(parse("(cons 'a (cons 'b (cons 'c '())))")).toString should be ("(a b c)")
      eval(parse("(car (cons 'a '(b c)))")).toString should be ("a")
      eval(parse("(cdr (cons 'a '(b c)))")).toString should be ("(b c)")
    }
    it("(cond (p1 e1) . . . (pn en)) is evaluated as follows; the p expressions are evaluated in order until one returns t; when one is found, the value of the corresponding e expression is returned as the value of the whole cond expression.") {
      eval(parse("(cond ((eq 'a 'b) 'first) ((atom 'a) 'second))")).toString should be ("second")
    }
  }
}