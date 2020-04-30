import org.scalatest.{FreeSpec, Matchers}

class BinarySearchSpec extends FreeSpec with Matchers {

  "Binary Search" - {

    "search 1" in {
      BinarySearch.search(List(1, 2, 3, 4, 6, 7), 10) shouldBe 5
    }

    "search 2" in {
      BinarySearch.search(List(1, 2, 3, 3, 4, 6, 7), 2) shouldBe 1
    }

    "search 3" in {
      BinarySearch.search(List(1, 2, 3, 4, 6, 7), 0) shouldBe -1
    }
  }
}
