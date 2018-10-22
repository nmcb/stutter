package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ExpressionAxioms extends FunSpec {
  describe("Expression axioms") {
    import Stutter._
    import Stutter.Parser._
    it("quote expressions should yield the quoted expression") {
      eval(parseLisp("(quote a)")).toString       should be ("a")
      eval(parseLisp("'a")).toString              should be ("a")
      eval(parseLisp("(quote (a b c))")).toString should be ("(a b c)")
    }
    it("atom expressions should yield the atom `t` if the argument yields an atom or the empty list") {
      eval(parseLisp("(atom 'a)")).toString        should be ("t")
      eval(parseLisp("(atom '())")).toString       should be ("t")
      eval(parseLisp("(atom (atom 'a))")).toString should be ("t")
    }
    it("atom expression should yield the empty list if the argument does not yield an atom or the empty list") {
      eval(parseLisp("(atom '(atom a))")).toString should be ("()")
      eval(parseLisp("(atom '(a b c))")).toString  should be ("()")
    }
    it("eq expression should yield the atom `t` if both arguments yield the same atom or both the empty list") {
      eval(parseLisp("(eq 'a 'a)")).toString   should be ("t")
      eval(parseLisp("(eq '() '())")).toString should be ("t")
    }
    it("eq expression should yield the empty list if both arguments do not yield the same atom or the empty list") {
      eval(parseLisp("(eq 'a 'b)")).toString should be ("()")
    }
    it("car expression should yield the first element of its argument list") {
      eval(parseLisp("(car '(a b c))")).toString should be ("a")
    }
    it("cdr expression should yield everything after the first element of its argument list") {
      eval(parseLisp("(cdr '(a b c))")).toString should be ("(b c)")
    }
    it("cons expression should return a list containing the value of its first argument followed by the elements of the value of its second argument") {
      eval(parseLisp("(cons 'a '(b c))")).toString should be ("(a b c)")
      eval(parseLisp("(cons 'a (cons 'b (cons 'c '())))")).toString should be ("(a b c)")
      eval(parseLisp("(car (cons 'a '(b c)))")).toString should be ("a")
      eval(parseLisp("(cdr (cons 'a '(b c)))")).toString should be ("(b c)")
    }
    it("cond expression (cond (p1 e1)...(pn en)) is evaluated as follows; the p expressions are evaluated in order until one returns t; when one is found, the value of the corresponding e expression is returned as the value of the whole cond expression.") {
      eval(parseLisp("(cond ((eq 'a 'b) 'first) ((atom 'a) 'second))")).toString should be ("second")
    }
  }
}
