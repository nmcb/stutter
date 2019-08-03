package splatter

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class PlatformTest extends FunSpec {

  describe("The platform") {
    it("should have the correct Java version") {
      System.getProperty("java.version") should startWith("12.0.2")
    }
    it("should have the correct Scala version") {
      scala.util.Properties.versionNumberString should be("2.12.8")
    }
  }
}
