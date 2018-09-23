import org.scalatest.{FreeSpec, Matchers}

class ChunkArraySpec extends FreeSpec with Matchers {

  "ChunkArray" - {

    "chunk" in {
      ChunkArray.chunk(Array(1, 2, 3, 4, 5, 6, 7), 3) shouldBe Array(Array(1, 2, 3), Array(4, 5, 6), Array(7))
      ChunkArray.chunk(Array(1, 2, 3, 4, 5, 6, 7), 2) shouldBe Array(Array(1, 2), Array(3, 4), Array(5, 6), Array(7))
      ChunkArray.chunk(Array(1, 2, 3, 4, 5), 3) shouldBe Array(Array(1, 2, 3), Array(4, 5))
    }
  }
}
