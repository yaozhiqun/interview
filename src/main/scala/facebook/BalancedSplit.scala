package facebook

object BalancedSplit extends App {

  def solve(xs: List[Int]): Boolean = {
    val sorted = xs.sorted
    sorted match {
      case Nil => false
      case _ :: Nil => false
      case head :: tail =>
        tail.zipWithIndex.foldLeft(head) { case (prev, (x, index)) =>
          if (x == prev) {
            x
          } else {
            if (sorted.take(index + 1).sum == sorted.takeRight(sorted.length - index - ).sum)
              return true
            else
              x
          }
        }
        false
    }
  }

  println(solve(List(1, 5, 7, 1)))
  println(solve(List(12, 7, 6, 7, 6)))
}
