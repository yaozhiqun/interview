package hankcerrank

object CountTriplets extends App {

  def solve(xs: List[Int], ratio: Int): List[(Int, Int, Int)] = {

    case class Node(value: Int, index: Int)
    case class Doubles(doubles: List[(Int, Int)] = Nil) {
      def genTriples(index: Int, triples: Triples): Triples = {
        triples.copy(triples = doubles.map(double => (double._1, double._2, index)).flatMap(_ :: triples.triples).reverse)
      }
    }
    case class Triples(triples: List[(Int, Int, Int)] = Nil)

    def recur(ys: List[Int], head: Node): Triples = {
      val first = head.value
      val second = first * ratio
      val third = second * ratio
      ys.zipWithIndex.foldLeft((Doubles(), Triples())) { case ((d, t), (y, index)) =>
        if (y == second) {
          (d.copy(doubles = (head.index, head.index + index + 1) :: d.doubles), t)
        } else if (y == third) {
          (d, d.genTriples(head.index + index + 1, t))
        } else {
          (d, t)
        }
      }._2
    }

    xs.zipWithIndex.foldLeft(List[(Int, Int, Int)]()) { case (l, (x, index)) =>
      if (index > xs.length - 2) {
        l
      } else {
        l ::: recur(xs.takeRight(xs.length - index - 1), Node(x, index)).triples
      }
    }
  }

  println(solve(List(1, 3, 9, 9, 27, 81), 3))
}

// 1 3 9 9 27 81