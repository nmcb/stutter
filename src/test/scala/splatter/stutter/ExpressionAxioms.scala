package splatter
package stutter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ExpressionAxioms extends AnyFunSpec {
  describe("Expression axioms") {
    import Stutter._
    import Stutter.Parser._
    it("quote expressions should yield the quoted expression") {
      eval("(quote a)").toString       should be ("a")
      eval("'a").toString              should be ("a")
      eval("(quote (a b c))").toString should be ("(a b c)")
    }
    it("atom expressions should yield the atom `t` if the argument yields an atom or the empty list") {
      eval("(atom 'a)").toString        should be ("t")
      eval("(atom '())").toString       should be ("t")
      eval("(atom (atom 'a))").toString should be ("t")
    }
    it("atom expression should yield the empty list if the argument does not yield an atom or the empty list") {
      eval("(atom '(atom a))").toString should be ("()")
      eval("(atom '(a b c))").toString  should be ("()")
    }
    it("eq expression should yield the atom `t` if both arguments yield the same atom or both the empty list") {
      eval("(eq 'a 'a)").toString   should be ("t")
      eval("(eq '() '())").toString should be ("t")
    }
    it("eq expression should yield the empty list if both arguments do not yield the same atom or the empty list") {
      eval("(eq 'a 'b)").toString should be ("()")
    }
    it("car expression should yield the first element of its argument list") {
      eval("(car '(a b c))").toString should be ("a")
    }
    it("cdr expression should yield everything after the first element of its argument list") {
      eval("(cdr '(a b c))").toString should be ("(b c)")
    }
    it("cons expression should return a list containing the value of its first argument followed by the elements of the value of its second argument") {
      eval("(cons 'a '(b c))").toString should be ("(a b c)")
      eval("(cons 'a (cons 'b (cons 'c '())))").toString should be ("(a b c)")
      eval("(car (cons 'a '(b c)))").toString should be ("a")
      eval("(cdr (cons 'a '(b c)))").toString should be ("(b c)")
    }
    it("cond expression (cond (p1 e1)...(pn en)) is evaluated as follows; the p expressions are evaluated in order until one returns t; when one is found, the value of the corresponding e expression is returned as the value of the whole cond expression.") {
      eval("(cond ((eq 'a 'b) 'first) ((atom 'a) 'second))").toString should be ("second")
    }
  }
}
