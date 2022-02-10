package splatter.stutter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class PrimitiveExpressions extends AnyFunSpec {
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
            case Lisp(Seq(AtomOp, _))    => AtomOp.value
            case Lisp(Seq(QuoteOp, _))   => QuoteOp.value
            case Lisp(Seq(EqOp, _, _))   => EqOp.value
            case Lisp(Seq(CarOp, _))     => CarOp.value
            case Lisp(Seq(CdrOp, _))     => CdrOp.value
            case Lisp(Seq(ConsOp, _, _)) => ConsOp.value
            case Lisp(CondOp +: _)       => CondOp.value
            case _ => sys.error(s"unmatched test expression $e")
          }
        ) should be (e.asInstanceOf[Lisp].expressions.head.asInstanceOf[Atom].value)
      )
    }
    it("quote expressions should be constructable") {
      Lisp(Seq(QuoteOp, Atom("foo"))) should be (parseLisp("(quote foo)"))
    }
    it("quote expressions should be extractable") {
      ((parseLisp("(quote foo)") : @unchecked) match { case Lisp(Seq(QuoteOp, foo)) => foo }) should be (parseLisp("foo"))
    }
    it("atom expressions should be constructable") {
      Lisp(Seq(AtomOp, Atom("foo"))) should be (parseLisp("(atom foo)"))
    }
    it("atom expressions should be extractable") {
      ((parseLisp("(atom foo)") : @unchecked) match { case Lisp(Seq(AtomOp, foo)) => foo }) should be (parseLisp("foo"))
    }
    it("eq expressions should be constructable") {
      Lisp(Seq(EqOp, Atom("foo"), Atom("bar"))) should be (parseLisp("(eq foo bar)"))
    }
    it("eq expressions should be extractable") {
      ((parseLisp("(eq foo bar)") : @unchecked) match { case Lisp(Seq(EqOp, foo, bar)) => (foo,bar) }) should be ((parseLisp("foo"), parseLisp("bar")))
    }
    it("car expressions should be constructable") {
      Lisp(Seq(CarOp, Lisp(Seq(Atom("foo"), Atom("bar"))))) should be (parseLisp("(car (foo bar))"))
    }
    it("car expressions should be extractable") {
      ((parseLisp("(car (foo bar))") : @unchecked) match { case Lisp(Seq(CarOp, arg)) => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cdr expressions should be constructable") {
      Lisp(Seq(CdrOp, (Lisp(Seq(Atom("foo"), Atom("bar")))))) should be (parseLisp("(cdr (foo bar))"))
    }
    it("cdr expressions should be extractable") {
      ((parseLisp("(cdr (foo bar))") : @unchecked) match { case Lisp(Seq(CdrOp, arg)) => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cons expressions should be constructable") {
      Lisp(Seq(ConsOp, Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be (parseLisp("(cons (foo) (bar))"))
    }
    it("cons expressions should be extractable") {
      ((parseLisp("(cons (foo) (bar))") : @unchecked) match { case Lisp(Seq(ConsOp, l1, l2)) => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
    }
  }
}
