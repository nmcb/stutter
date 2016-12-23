package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ExpressionAxioms extends FunSpec {
  describe("Expression Axioms") {
    import Expr.Axioms._
    it("QuoteExpression should yield the quoted expression") {
      yields(QuoteExpr(Atom("a"))) should be {
        Atom("a")
      }
    }
    it("AtomExpression should yield the atom 't' if the argument is an atom or the empty list") {
      yields(AtomExpr(Atom("a"))) should be {
        Expr.t
      }
      yields(AtomExpr(Expr.EmptyLisp)) should be {
        Expr.t
      }
    }
    it("AtomExpression should yield the empty list if the argument is not an atom or the empty list") {
      yields(AtomExpr(Lisp(Seq(Atom("a"))))) should be {
        Expr.EmptyLisp
      }
    }
  }
}