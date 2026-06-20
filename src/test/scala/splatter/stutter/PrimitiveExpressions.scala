package splatter
package stutter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class PrimitiveExpressions extends AnyFunSpec:
  
  import Parser.*
  
  describe("Primitive expressions"):
    it("should be differentiable"):
      Seq(
        "(atom foo)".parse,
        "(quote bar)".parse,
        "(eq foo bar)".parse,
        "(car (foo bar))".parse,
        "(cdr (foo bar))".parse,
        "(cons (foo bar) (baz))".parse,
        "(cond (eq ('a 'b) 'first) (eq ('a 'a) ('second)))".parse
      ) foreach (
        e => (
          e.runtimeChecked match
            case Lisp(Seq(AtomLit.Op, _))    => AtomLit.Op.value
            case Lisp(Seq(QuoteLit.Op, _))   => QuoteLit.Op.value
            case Lisp(Seq(EqLit.Op, _, _))   => EqLit.Op.value
            case Lisp(Seq(CarLit.Op, _))     => CarLit.Op.value
            case Lisp(Seq(CdrLit.Op, _))     => CdrLit.Op.value
            case Lisp(Seq(ConsLit.Op, _, _)) => ConsLit.Op.value
            case Lisp(CondLit.Op +: _)       => CondLit.Op.value
        ) should be (e.asInstanceOf[Lisp].subs.head.asInstanceOf[Atom].value)
      )

    it("quote expressions should be constructable"):
      Lisp(Seq(QuoteLit.Op, Atom("foo"))) should be ("(quote foo)".parse)

    it("quote expressions should be extractable"):
      "(quote foo)".parse.runtimeChecked match
        case Lisp(Seq(QuoteLit.Op, foo)) => foo should be ("foo".parse)
      "(quote foo)".parse.runtimeChecked match
        case QuoteLit(foo)               => foo should be ("foo".parse)

    it("atom expressions should be constructable"):
      Lisp(Seq(AtomLit.Op, Atom("foo"))) should be ("(atom foo)".parse)

    it("atom expressions should be extractable"):
      "(atom foo)".parse.runtimeChecked match
        case Lisp(Seq(AtomLit.Op, foo)) => foo should be ("foo".parse)
      "(atom foo)".parse.runtimeChecked match
        case AtomLit(foo)               => foo should be ("foo".parse)

    it("eq expressions should be constructable"):
      Lisp(Seq(EqLit.Op, Atom("foo"), Atom("bar"))) should be ("(eq foo bar)".parse)

    it("eq expressions should be extractable"):
      "(eq foo bar)".parse.runtimeChecked match
        case Lisp(Seq(EqLit.Op, foo, bar)) => (foo,bar) should be ("foo".parse, "bar".parse)
      "(eq foo bar)".parse.runtimeChecked match
        case EqLit(foo, bar)               => (foo,bar) should be ("foo".parse, "bar".parse)

    it("car expressions should be constructable"):
      Lisp(Seq(CarLit.Op, Lisp(Seq(Atom("foo"), Atom("bar"))))) should be ("(car (foo bar))".parse)

    it("car expressions should be extractable"):
      "(car (foo bar))".parse.runtimeChecked match
        case Lisp(Seq(CarLit.Op, arg)) => arg should be ("(foo bar)".parse)
      "(car (foo bar))".parse.runtimeChecked match
        case CarLit(arg)               => arg should be ("(foo bar)".parse)

    it("cdr expressions should be constructable"):
      Lisp(Seq(CdrLit.Op, (Lisp(Seq(Atom("foo"), Atom("bar")))))) should be ("(cdr (foo bar))".parse)

    it("cdr expressions should be extractable"):
      "(cdr (foo bar))".parse.runtimeChecked match
        case Lisp(Seq(CdrLit.Op, arg)) => arg should be ("(foo bar)".parse)
      "(cdr (foo bar))".parse.runtimeChecked match
        case CdrLit(arg)               => arg should be ("(foo bar)".parse)

    it("cons expressions should be constructable"):
      Lisp(Seq(ConsLit.Op, Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be ("(cons (foo) (bar))".parse)

    it("cons expressions should be extractable"):
      "(cons (foo) (bar))".parse.runtimeChecked match
        case Lisp(Seq(ConsLit.Op, l1, l2)) => (l1, l2) should be ("(foo)".parse, "(bar)".parse)
      "(cons (foo) (bar))".parse.runtimeChecked match
        case ConsLit(l1, l2)               => (l1, l2) should be ("(foo)".parse, "(bar)".parse)

    it("cond expressions should be constructable"):
      Lisp(Seq(CondLit.Op, Lisp(Seq(Atom("foo"))), Lisp(Seq(Atom("bar"))))) should be ("(cond (foo) (bar))".parse)

    it("cond expressions should be extractable"):
      "(cond (foo) (bar))".parse.runtimeChecked match
        case Lisp(Seq(CondLit.Op, l1, l2)) => (l1, l2) should be ("(foo)".parse, "(bar)".parse)
      "(cond (foo) (bar))".parse.runtimeChecked match
        case CondLit(Seq(l1, l2))          => (l1, l2) should be ("(foo)".parse, "(bar)".parse)
