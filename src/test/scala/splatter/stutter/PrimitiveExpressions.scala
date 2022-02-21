package splatter
package stutter

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
            case Lisp(Seq(Atom.Op, _))    => Atom.Op.value
            case Lisp(Seq(Quote.Op, _))   => Quote.Op.value
            case Lisp(Seq(Eq.Op, _, _))   => Eq.Op.value
            case Lisp(Seq(Car.Op, _))     => Car.Op.value
            case Lisp(Seq(Cdr.Op, _))     => Cdr.Op.value
            case Lisp(Seq(Cons.Op, _, _)) => Cons.Op.value
            case Lisp(Cond.Op +: _)       => Cond.Op.value
            case _ => sys.error(s"unmatched test expression $e")
          }
        ) should be (e.asInstanceOf[Lisp].subs.head.asInstanceOf[Atom].value)
      )
    }
    it("quote expressions should be constructable") {
      Lisp(Seq(Quote.Op, Atom("foo"))) should be (parseLisp("(quote foo)"))
    }
    it("quote expressions should be extractable") {
      ((parseLisp("(quote foo)") : @unchecked) match { case Lisp(Seq(Quote.Op, foo)) => foo }) should be (parseLisp("foo"))
      ((parseLisp("(quote foo)") : @unchecked) match { case Quote(foo)               => foo }) should be (parseLisp("foo"))
    }
    it("atom expressions should be constructable") {
      Lisp(Seq(Atom.Op, Atom("foo"))) should be (parseLisp("(atom foo)"))
    }
    it("atom expressions should be extractable") {
      ((parseLisp("(atom foo)") : @unchecked) match { case Lisp(Seq(Atom.Op, foo)) => foo }) should be (parseLisp("foo"))
      ((parseLisp("(atom foo)") : @unchecked) match { case Atom(foo)               => foo }) should be (parseLisp("foo"))
    }
    it("eq expressions should be constructable") {
      Lisp(Seq(Eq.Op, Atom("foo"), Atom("bar"))) should be (parseLisp("(eq foo bar)"))
    }
    it("eq expressions should be extractable") {
      ((parseLisp("(eq foo bar)") : @unchecked) match { case Lisp(Seq(Eq.Op, foo, bar)) => (foo,bar) }) should be ((parseLisp("foo"), parseLisp("bar")))
      ((parseLisp("(eq foo bar)") : @unchecked) match { case Eq(foo, bar)               => (foo,bar) }) should be ((parseLisp("foo"), parseLisp("bar")))
    }
    it("car expressions should be constructable") {
      Lisp(Seq(Car.Op, Lisp(Seq(Atom("foo"), Atom("bar"))))) should be (parseLisp("(car (foo bar))"))
    }
    it("car expressions should be extractable") {
      ((parseLisp("(car (foo bar))") : @unchecked) match { case Lisp(Seq(Car.Op, arg)) => arg }) should be (parseLisp("(foo bar)"))
      ((parseLisp("(car (foo bar))") : @unchecked) match { case Car(arg)               => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cdr expressions should be constructable") {
      Lisp(Seq(Cdr.Op, (Lisp(Seq(Atom("foo"), Atom("bar")))))) should be (parseLisp("(cdr (foo bar))"))
    }
    it("cdr expressions should be extractable") {
      ((parseLisp("(cdr (foo bar))") : @unchecked) match { case Lisp(Seq(Cdr.Op, arg)) => arg }) should be (parseLisp("(foo bar)"))
      ((parseLisp("(cdr (foo bar))") : @unchecked) match { case Cdr(arg)               => arg }) should be (parseLisp("(foo bar)"))
    }
    it("cons expressions should be constructable") {
      Lisp(Seq(Cons.Op, Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be (parseLisp("(cons (foo) (bar))"))
    }
    it("cons expressions should be extractable") {
      ((parseLisp("(cons (foo) (bar))") : @unchecked) match { case Lisp(Seq(Cons.Op, l1, l2)) => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
      ((parseLisp("(cons (foo) (bar))") : @unchecked) match { case Cons(l1, l2)               => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
    }
    it("cond expressions should be constructable") {
      Lisp(Seq(Cond.Op, Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be (parseLisp("(cond (foo) (bar))"))
    }
    it("cond expressions should be extractable") {
      ((parseLisp("(cond (foo) (bar))") : @unchecked) match { case Lisp(Seq(Cond.Op, l1, l2)) => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
      ((parseLisp("(cond (foo) (bar))") : @unchecked) match { case Cond(Seq(l1, l2))          => (l1, l2) }) should be ((parseLisp("(foo)"), parseLisp("(bar)")))
    }
  }
}
