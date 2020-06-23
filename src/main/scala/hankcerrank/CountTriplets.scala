package hankcerrank

object CountTriplets extends App {

  def solve(xs: List[Int], ratio: Int): List[(Int, Int, Int)] = {

    def searchTriples(ys: List[Int], first: Int, firstIndex: Int,
            triples: List[(Int, Int, Int)],
            doubles: List[(Int, Int)] = Nil): List[(Int, Int, Int)] = {
      ys.zipWithIndex.foldLeft((doubles, triples)) { case ((d, t), (y, index)) =>
        if (y == first * ratio) {
          (d :+ (firstIndex, index + firstIndex + 1), t)
        } else if (y == first * ratio * ratio) {
          (d, t ::: d.map(d => (d._1, d._2, index + firstIndex + 1)))
        } else {
          (d, t)
        }
      }
    }._2

    xs.zipWithIndex.foldLeft(List[(Int, Int, Int)]()) { case (triples, (x, index)) =>
      searchTriples(xs.takeRight(xs.length - index - 1), x, index, triples)
    }
  }

  println(solve(List(1, 3, 9, 9, 27, 81), 3))
}

// 1 3 9 9 27 81