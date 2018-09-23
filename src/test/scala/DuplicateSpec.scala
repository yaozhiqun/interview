import org.scalatest.{FreeSpec, Matchers}

class DuplicateSpec extends FreeSpec with Matchers {

  "#duplicate" - {

    "return a list of duplicate numbers" in {
      Duplicate.duplicate(List(1, 2, 3, 4, 5, 3, 2, 3, 4, 5, 6, 4, 5)) shouldBe Set(2, 3, 4, 5)
      Duplicate.duplicate(List(1, 1, 2, 3, 4, 5, 3, 2, 3, 4, 5, 6, 4, 5)) shouldBe Set(2, 3, 4, 5, 1)
    }
  }
}
