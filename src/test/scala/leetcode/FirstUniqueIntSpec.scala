package leetcode

import org.scalatest.{FreeSpec, Matchers}

class FirstUniqueIntSpec extends FreeSpec with Matchers {

  "FindFirstUniqueInt" - {

    "find" in {
      FirstUniqueInt.find(List(2, 3, 5, 5, 2)) shouldBe Some(3)
      FirstUniqueInt.find(List(2, 3, 5, 5, 2, 3)) shouldBe None
    }
  }
}
