package facebook

object MinimizingPermutations extends App {

  def solve(xs: List[Int]): Int = {
    val dest = xs.sorted
    val traversed = scala.collection.mutable.ListBuffer[List[Int]]()

    def recur(xss: List[List[Int]], round: Int = 0): Int = {
      if (xss.contains(dest)) {
        round
      } else {
        traversed ++= xss
        val toTraverse = xss.flatMap { xs =>
          (2 to xs.length) flatMap { size =>
            xs.sliding(size).zipWithIndex.foldLeft(List[List[Int]]()) { case (ll, (x, i)) =>
              val l = xs.take(i) ::: x.reverse ::: xs.takeRight(xs.length - size - i)
              if (traversed.contains(l)) ll else l :: ll
            }
          }
        }
        recur(toTraverse, round + 1)
      }
    }
    recur(List(xs))
  }

  println(solve(List(3, 1, 2, 4)))
  println(solve(List(3, 1, 2)))
  println(solve(List(3, 2, 1)))
  println(solve(List(1, 2, 3)))
}
