package facebook

object Reversed extends App {

  def solve(xs: List[Int], ys: List[Int]): Boolean = {
    if (xs.length == ys.length) {
      def recur(left: Int, right: Int): Boolean = {
        if (left <= right)
          xs(left) == ys(right) && recur(left + 1, right - 1)
        else
          true
      }
      recur(0, xs.length - 1)
    } else {
      false
    }
  }

  println(solve(List(1, 2), List(2, 1)))
  println(solve(List(1, 2, 3), List(3, 2, 1)))
  println(solve(List(2), List(1)))
}
