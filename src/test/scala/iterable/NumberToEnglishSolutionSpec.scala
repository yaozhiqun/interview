package iterable

import org.scalatest.{FreeSpec, Matchers}

class NumberToEnglishSolutionSpec extends FreeSpec with Matchers {

  "NumberToEnglish" - {

    "translate" in {
      NumberToEnglishSolution.transfer(10) shouldBe "ten"
      NumberToEnglishSolution.transfer(11) shouldBe "eleven"
      NumberToEnglishSolution.transfer(12) shouldBe "twelve"
      NumberToEnglishSolution.transfer(20) shouldBe "twenty"
    }
  }

}
