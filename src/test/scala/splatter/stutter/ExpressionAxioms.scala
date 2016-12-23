package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ExpressionAxioms extends FunSpec {
  describe("Expression Axioms") {
    import Stutter.Axioms._
    import Stutter.Parser._
    it("quote expressions should yield the quoted expression") {
      yields(parse("(quote a)")).toString       should be ("a")
      yields(parse("'a")).toString              should be ("a")
      yields(parse("(quote (a b c))")).toString should be ("(a b c)")
    }
    it("atom expressions should yield the atom `t` if the argument yields an atom or the empty list") {
      yields(parse("(atom 'a)")).toString        should be ("t")
      yields(parse("(atom '())")).toString       should be ("t")
      yields(parse("(atom (atom 'a))")).toString should be ("t")
    }
    it("atom expression should yield the empty list if the argument does not yield an atom or the empty list") {
      yields(parse("(atom '(atom 'a))")).toString should be ("()")
      yields(parse("(atom '(a b c))")).toString   should be ("()")
    }
    it("eq expression should yield the atom `t` if both operands yield the same atom or both the empty list") {
      yields(parse("(eq 'a 'a)")).toString should be ("t")
      yields(parse("(eq '() '())")).toString should be ("t")
    }
    it("eq expression should yield the empty list if both operands do not yield the same atom or both the empty list") {
      yields(parse("(eq 'a 'b)")).toString should be ("()")
    }
  }
}