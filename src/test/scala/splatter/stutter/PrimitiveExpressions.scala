package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class PrimitiveExpressions extends FunSpec {
  import Stutter._
  import Stutter.Parser._
  describe("Primitive expressions") {
    it("should be differentiable") {
      Seq(
        parseLisp("(atom foo)"),
        parseLisp("(quote bar)"),
        parseLisp("(eq foo bar)"),
        parseLisp("(car (foo bar))"),
        parseLisp("(cdr (foo bar))"),
        parseLisp("(cons (foo bar) (baz))"),
        parseLisp("(cond (eq ('a 'b) 'first) (eq ('a 'a) ('second)))")
      ) foreach (
        e => (
          e match {
            case AtomExpr(_)   => AtomOp.value
            case QuoteExpr(_)  => QuoteOp.value
            case EqExpr(_,_)   => EqOp.value
            case CarExpr(_)    => CarOp.value
            case CdrExpr(_)    => CdrOp.value
            case ConsExpr(_,_) => ConsOp.value
            case CondExpr(_)   => CondOp.value
          }
        ) should be (e.asInstanceOf[Lisp].expressions.head.asInstanceOf[Atom].value)
      )
    }
    it("quote expressions should be constructable") {
      QuoteExpr(Atom("foo")) should be (parseLisp("(quote foo)"))
    }
    it("quote expressions should be extractable") {
      (parseLisp("(quote foo)") match { case QuoteExpr(foo) => foo }) should be (parseLisp("foo"))
    }
    it("atom expressions should be constructable") {
      AtomExpr(Atom("foo")) should be (parseLisp("(atom foo)"))
    }
    it("atom expressions should be extractable") {
      (parseLisp("(atom foo)") match { case AtomExpr(foo) => foo }) should be (parseLisp("foo"))
    }
    it("eq expressions should be constructable") {
      EqExpr((Atom("foo"), Atom("bar"))) should be (parseLisp("(eq foo bar)"))
    }
    it("eq expressions should be extractable") {
      (parseLisp("(eq foo bar)") match { case EqExpr(foo, bar) => (foo,bar) }) should be ((parseLisp("foo"), parseLisp("bar")))
    }
    it("car expressions should be constructable") {
      CarExpr(Lisp(Seq(Atom("foo"), Atom("bar")))) should be (parseLisp("(car (foo bar))"))
    }
    it("car expressions should be extractable") {
      (parseLisp("(car (foo bar))") match { case CarExpr(arg) => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cdr expressions should be constructable") {
      CdrExpr(Lisp(Seq(Atom("foo"), Atom("bar")))) should be (parseLisp("(cdr (foo bar))"))
    }
    it("cdr expressions should be extractable") {
      (parseLisp("(cdr (foo bar))") match { case CdrExpr(arg) => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cons expressions should be constructable") {
      ConsExpr((Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be (parseLisp("(cons (foo) (bar))"))
    }
    it("cons expressions should be extractable") {
      (parseLisp("(cons (foo) (bar))") match { case ConsExpr(l1, l2) => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
    }
  }
}
