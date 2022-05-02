package splatter
package stutter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class PrimitiveExpressions extends AnyFunSpec:
  import Parser._
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
            case Lisp(Seq(AtomLit.Op, _))    => AtomLit.Op.value
            case Lisp(Seq(QuoteLit.Op, _))   => QuoteLit.Op.value
            case Lisp(Seq(EqLit.Op, _, _))   => EqLit.Op.value
            case Lisp(Seq(CarLit.Op, _))     => CarLit.Op.value
            case Lisp(Seq(CdrLit.Op, _))     => CdrLit.Op.value
            case Lisp(Seq(ConsLit.Op, _, _)) => ConsLit.Op.value
            case Lisp(CondLit.Op +: _)       => CondLit.Op.value
            case _ => sys.error(s"unmatched test expression $e")
          }
        ) should be (e.asInstanceOf[Lisp].subs.head.asInstanceOf[Atom].value)
      )
    }
    it("quote expressions should be constructable") {
      Lisp(Seq(QuoteLit.Op, Atom("foo"))) should be (parseLisp("(quote foo)"))
    }
    it("quote expressions should be extractable") {
      ((parseLisp("(quote foo)") : @unchecked) match { case Lisp(Seq(QuoteLit.Op, foo)) => foo }) should be (parseLisp("foo"))
      ((parseLisp("(quote foo)") : @unchecked) match { case QuoteLit(foo)               => foo }) should be (parseLisp("foo"))
    }
    it("atom expressions should be constructable") {
      Lisp(Seq(AtomLit.Op, Atom("foo"))) should be (parseLisp("(atom foo)"))
    }
    it("atom expressions should be extractable") {
      ((parseLisp("(atom foo)") : @unchecked) match { case Lisp(Seq(AtomLit.Op, foo)) => foo }) should be (parseLisp("foo"))
      ((parseLisp("(atom foo)") : @unchecked) match { case AtomLit(foo)               => foo }) should be (parseLisp("foo"))
    }
    it("eq expressions should be constructable") {
      Lisp(Seq(EqLit.Op, Atom("foo"), Atom("bar"))) should be (parseLisp("(eq foo bar)"))
    }
    it("eq expressions should be extractable") {
      ((parseLisp("(eq foo bar)") : @unchecked) match { case Lisp(Seq(EqLit.Op, foo, bar)) => (foo,bar) }) should be ((parseLisp("foo"), parseLisp("bar")))
      ((parseLisp("(eq foo bar)") : @unchecked) match { case EqLit(foo, bar)               => (foo,bar) }) should be ((parseLisp("foo"), parseLisp("bar")))
    }
    it("car expressions should be constructable") {
      Lisp(Seq(CarLit.Op, Lisp(Seq(Atom("foo"), Atom("bar"))))) should be (parseLisp("(car (foo bar))"))
    }
    it("car expressions should be extractable") {
      ((parseLisp("(car (foo bar))") : @unchecked) match { case Lisp(Seq(CarLit.Op, arg)) => arg }) should be (parseLisp("(foo bar)"))
      ((parseLisp("(car (foo bar))") : @unchecked) match { case CarLit(arg)               => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cdr expressions should be constructable") {
      Lisp(Seq(CdrLit.Op, (Lisp(Seq(Atom("foo"), Atom("bar")))))) should be (parseLisp("(cdr (foo bar))"))
    }
    it("cdr expressions should be extractable") {
      ((parseLisp("(cdr (foo bar))") : @unchecked) match { case Lisp(Seq(CdrLit.Op, arg)) => arg }) should be (parseLisp("(foo bar)"))
      ((parseLisp("(cdr (foo bar))") : @unchecked) match { case CdrLit(arg)               => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cons expressions should be constructable") {
      Lisp(Seq(ConsLit.Op, Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be (parseLisp("(cons (foo) (bar))"))
    }
    it("cons expressions should be extractable") {
      ((parseLisp("(cons (foo) (bar))") : @unchecked) match { case Lisp(Seq(ConsLit.Op, l1, l2)) => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
      ((parseLisp("(cons (foo) (bar))") : @unchecked) match { case ConsLit(l1, l2)               => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
    }
    it("cond expressions should be constructable") {
      Lisp(Seq(CondLit.Op, Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be (parseLisp("(cond (foo) (bar))"))
    }
    it("cond expressions should be extractable") {
      ((parseLisp("(cond (foo) (bar))") : @unchecked) match { case Lisp(Seq(CondLit.Op, l1, l2)) => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
      ((parseLisp("(cond (foo) (bar))") : @unchecked) match { case CondLit(Seq(l1, l2))          => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
    }
  }
