import org.scalatest.{Matchers, FreeSpec}

class CompressSpec extends FreeSpec with Matchers {

  "Compress#compress" - {

    "should compress string" in {
      val str = "1aaaaabcccddd"
      Compress.compress(str) shouldBe "11a5b1c3d3"
    }

    "should compress empty string" in {
      Compress.compress("") shouldBe ""
    }

    "should compress string with space" in {
      Compress.compress(" ") shouldBe " 1"
    }
  }
}
