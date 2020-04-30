import org.scalatest.{FreeSpec, Matchers}

class FindFirstNonRepeatingCharSpec extends FreeSpec with Matchers {

  "#findFirstNonRepeatingChar" - {

    "test1" in {
      FindFirstNonRepeatingChar.firstNonRepeat("lloword") shouldBe Some('w')
    }

    "test2" in {
      FindFirstNonRepeatingChar.firstNonRepeat("hello word") shouldBe Some('h')
    }
  }
}
