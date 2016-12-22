package splatter
package stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ParserLaws extends FunSpec {
  describe("Parser") {
    it("should parse atoms") {
      Parser.parse("foo")   should be (Atom("foo"))
      Parser.parse(" foo ") should be (Atom("foo"))
      Parser.parse("FOO")   should be (Atom("FOO"))
    }
  }
}
