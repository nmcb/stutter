package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class Axioms extends FunSpec {
  describe("Lisp axioms") {
    import Expr._
    it("quote should yield the quoted expression") {
      yields(Quote(Atom("a"))) should be(
        Atom("a")
      )
      yields(Quote(Lisp(Seq(Atom("a"), Atom("a"), Atom("a"))))) should be(
        Lisp(Seq(Atom("a"), Atom("a"), Atom("a")))
      )
    }
  }
}
