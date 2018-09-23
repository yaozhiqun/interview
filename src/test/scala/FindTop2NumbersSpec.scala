import org.scalatest.{FreeSpec, FunSuite, Matchers}

class FindTop2NumbersSpec extends FreeSpec with Matchers {

  "Find top 2 numbers" - {

    "test1" in {
      FindTop2Numbers.find(List(1, 2, 10, 55, 100, 3, 2, 1)) shouldBe (100, 55)
    }

    "test2" in {
      FindTop2Numbers.find(List(1, 2, 3, 4, 5)) shouldBe (5, 4)
    }
  }
}
