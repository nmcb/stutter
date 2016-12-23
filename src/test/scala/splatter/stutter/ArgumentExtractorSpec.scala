package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ArgumentExtractorSpec extends FunSpec {
  import Stutter.Axioms._
  import Stutter.Parser._
  describe("ArgumentExtractor instances") {
    it("should be differentiable") {
      Seq(
        parse("(atom foo)"),
        parse("(quote bar)")
      ) foreach (
        e => (
          e match {
            case AtomExpr(_)  => AtomOperator.value
            case QuoteExpr(_) => QuoteOperator.value
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
  }
}
