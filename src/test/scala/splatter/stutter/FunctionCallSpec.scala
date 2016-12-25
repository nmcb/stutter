package splatter.stutter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class FunctionCallSpec extends FunSpec {
  import Stutter._
  import Stutter.Parser._
  describe("A function call") {
    it("should work as specified...") {
      eval(
        parse(
          """
            |  ((lambda (x) (cons x '(b)))
            |    'a)
            |
          """.stripMargin
        )
      ) should be (parse("(a b)"))

      eval(
        parse(
          """
            |  ((lambda (x y) (cons x (cdr y)))
            |    'z
            |    '(a b c))
            |
          """.stripMargin
        )
      ) should be (parse("(z b c)"))
    }
    it("treat parameters as operators in expressions as well as arguments") {
      eval(
        parse(
          """
          |  ((lambda (f) (f '(b c)))
          |    ’(lambda (x) (cons 'a x)))
          |
        """.stripMargin
        )
      ) should be (parse("(a b c)"))
    }
  }
}