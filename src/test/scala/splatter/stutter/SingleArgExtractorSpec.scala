package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class SingleArgExtractorSpec extends FunSpec {
  import Expr.Axioms._
  describe("SingleArgExtractor instances") {
    it("should be differentiable") {
      Seq(AtomExpr(Expr.t), QuoteExpr(Expr.t)) foreach (
        e => (
          e match {
            case AtomExpr(_)  => AtomOperator.value
            case QuoteExpr(_) => QuoteOperator.value
          }
        ) should be (e.expressions.head.asInstanceOf[Atom].value)
      )
    }
    it("quote expressions should be constructable") {
      QuoteExpr(Atom("foo")) should be {
        Lisp(Seq(QuoteOperator, Atom("foo")))
      }
    }
    it("AtomExpr should be constructable") {
      AtomExpr(Atom("foo")) should be {
        Lisp(Seq(AtomOperator, Atom("foo")))
      }
    }
    it("QuoteExpr should be extractable") {
      (QuoteExpr(Atom("foo")) match { case QuoteExpr(a) => a }) should be {
        Atom("foo")
      }
    }
    it("AtomExpr should be extractable") {
      (AtomExpr(Atom("foo")) match { case AtomExpr(a) => a }) should be {
        Atom("foo")
      }
    }
  }
}
