import org.scalatest.{FreeSpec, Matchers}

class SpiralSpec extends FreeSpec with Matchers {

  "Spiral matrix" - {

    "test 1" in {
      Spiral.matrix(2) shouldBe Array(
        Array(1, 2),
        Array(4, 3))
    }

    "test 2" in {
      Spiral.matrix(3) shouldBe Array(
        Array(1, 2, 3),
        Array(8, 9, 4),
        Array(7, 6, 5))
    }

    "test 3" in {
      Spiral.matrix(4) shouldBe Array(
        Array(1,  2,  3,  4),
        Array(12, 13, 14, 5),
        Array(11, 16, 15, 6),
        Array(10, 9,  8,  7)
      )
    }

    "test 4" in {
      Spiral.matrix(5) shouldBe Array(
        Array(1,  2,  3,  4,  5),
        Array(16, 17, 18, 19, 6),
        Array(15, 24, 25, 20, 7),
        Array(14, 23, 22, 21, 8),
        Array(13, 12, 11, 10, 9)
      )
    }
  }
}
