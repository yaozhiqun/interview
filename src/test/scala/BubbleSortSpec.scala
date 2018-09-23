import org.scalatest.{FreeSpec, FunSuite, Matchers}
import sort.BubbleSort

class BubbleSortSpec extends FreeSpec with Matchers{

  "Bubble sorting" - {

    "sort" in {
      BubbleSort.sort(List(1, 4, 5, 2, 3, 0, 3, 5, 7, 9)) shouldBe List(0, 1, 2, 3, 3, 4, 5, 5, 7, 9)
    }
  }
}
