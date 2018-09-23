import org.scalatest.{FreeSpec, FunSuite, Matchers}
import sort.InsertionSort

class InsertionSortSpec extends FreeSpec with Matchers {

  "insertion sort" - {

    "sort" in {
      InsertionSort.sort(List(5,3,1,2)) shouldBe List(1,2, 3, 5)
    }
  }
}
