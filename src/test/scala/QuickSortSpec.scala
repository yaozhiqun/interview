import org.scalatest.{FreeSpec, FunSuite, Matchers}
import sort.QuickSort

class QuickSortSpec extends FreeSpec with Matchers {

  "Quick Sort" - {

    "sort" in {
      QuickSort.sort(List(10, 2, 3, 2, 9, 4, 5, 2, 1, 6)) shouldBe List(1, 2, 2, 2, 3, 4, 5, 6, 9, 10)
    }
  }
}
