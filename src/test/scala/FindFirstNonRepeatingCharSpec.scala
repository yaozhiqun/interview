import org.scalatest.{FreeSpec, Matchers}

class FindFirstNonRepeatingCharSpec extends FreeSpec with Matchers {

  "#findFirstNonRepeatingChar" - {

    "test1" in {
      FindFirstNonRepeatingChar.findFirstUniqueChar("lloword") shouldBe Some('w')
    }

    "test2" in {
      FindFirstNonRepeatingChar.findFirstUniqueChar("hello word") shouldBe Some('h')
    }
  }
}
