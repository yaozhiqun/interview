package flexport.reservation

import org.scalatest.{FreeSpec, Matchers}

class ShortestPatternTest extends FreeSpec with Matchers {

  "findPattern" - {

    "test 1" in {
      "ABC" shouldBe ShortestPattern.findPattern("ABCABCA")
    }

    "test 2" in {
      "ABAC" shouldBe ShortestPattern.findPattern("ABACABACABA")
    }

    "test 3" in {
      "ABACD" shouldBe ShortestPattern.findPattern("ABACDABACDABA")
    }

    "test 4" in {
      "ABABC" shouldBe ShortestPattern.findPattern("ABABCABABCA")
    }

    "test 5" in {
      "" shouldBe ShortestPattern.findPattern("")
    }
  }
}
