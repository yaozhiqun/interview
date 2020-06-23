package elevenessentials

object CommonElementsInTwoSorted extends App {

  def solve(a: Array[Int], b: Array[Int]): Array[Int] = {

    def rec(xs: List[Int], ys: List[Int], result: List[Int] = Nil): List[Int] = {
      (xs, ys) match {
        case (x :: xtail, y :: ytail) =>
          if (x == y)
            x :: rec(xtail, ytail) ::: result
          else if (x > y)
            rec(xs, ytail) ::: result
          else
            rec(xtail, ys) ::: result
        case _ =>
          result

      }
    }
    rec(a.toList, b.toList).toArray
  }

  println(solve(Array(1,3,4,6,7,9), Array(1,2,4,5,9,10)).deep)
}
