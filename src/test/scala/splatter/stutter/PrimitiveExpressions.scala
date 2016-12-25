package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class PrimitiveExpressions extends FunSpec {
  import Stutter._
  import Stutter.Parser._
  describe("Primitive expressions") {
    it("should be differentiable") {
      Seq(
        parse("(atom foo)"),
        parse("(quote bar)"),
        parse("(eq foo bar)"),
        parse("(car (foo bar))"),
        parse("(cdr (foo bar))"),
        parse("(cons (foo bar) (baz))"),
        parse("(cond (eq ('a 'b) 'first) (eq ('a 'a) ('second)))")
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
      QuoteExpr(Atom("foo")) should be (parse("(quote foo)"))
    }
    it("quote expressions should be extractable") {
      (parse("(quote foo)") match { case QuoteExpr(foo) => foo }) should be (parse("foo"))
    }
    it("atom expressions should be constructable") {
      AtomExpr(Atom("foo")) should be (parse("(atom foo)"))
    }
    it("atom expressions should be extractable") {
      (parse("(atom foo)") match { case AtomExpr(foo) => foo }) should be (parse("foo"))
    }
    it("eq expressions should be constructable") {
      EqExpr((Atom("foo"), Atom("bar"))) should be (parse("(eq foo bar)"))
    }
    it("eq expressions should be extractable") {
      (parse("(eq foo bar)") match { case EqExpr(foo, bar) => (foo,bar) }) should be ((parse("foo"), parse("bar")))
    }
    it("car expressions should be constructable") {
      CarExpr(Lisp(Seq(Atom("foo"), Atom("bar")))) should be (parse("(car (foo bar))"))
    }
    it("car expressions should be extractable") {
      (parse("(car (foo bar))") match { case CarExpr(arg) => arg }) should be (parse("(foo bar)"))
    }
    it("cdr expressions should be constructable") {
      CdrExpr(Lisp(Seq(Atom("foo"), Atom("bar")))) should be (parse("(cdr (foo bar))"))
    }
    it("cdr expressions should be extractable") {
      (parse("(cdr (foo bar))") match { case CdrExpr(arg) => arg }) should be (parse("(foo bar)"))
    }
    it("cons expressions should be constructable") {
      ConsExpr((Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be (parse("(cons (foo) (bar))"))
    }
    it("cons expressions should be extractable") {
      (parse("(cons (foo) (bar))") match { case ConsExpr(l1, l2) => (l1, l2) }) should be ((parse("(foo)"), parse("(bar)")))
    }
  }
}
