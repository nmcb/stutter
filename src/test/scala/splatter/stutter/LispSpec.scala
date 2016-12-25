package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class LispSpec extends FunSpec {
  import Stutter.Parser._
  describe("A lisp list") {
    it("should be able to replace parameters recursively") {
      parse("(a b (c d (e) f) (g) h)").replace(Map(
        Atom("a") -> Atom("A"),
        Atom("b") -> Atom("B"),
        Atom("c") -> Atom("C"),
        Atom("d") -> Atom("D"),
        Atom("e") -> Atom("E"),
        Atom("f") -> Atom("F"),
        Atom("g") -> Atom("G"),
        Atom("h") -> Atom("H")
      )) should be (parse("(A B (C D (E) F) (G) H)"))
    }
  }
}
