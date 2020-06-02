package facebook

object Reversed extends App {

  def solve(xs: List[Int], ys: List[Int]): Boolean = {
    if (xs.length == ys.length) {
      def recur(n: Int, m: Int): Boolean = {
        if (n <= m)
          xs(n) == ys(m) && recur(n + 1, m - 1)
        else
          true
      }
      recur(0, xs.length - 1)
    } else {
      false
    }
  }

  println(solve(List(1, 2), List(2, 1)))
  println(solve(List(2), List(1)))
}
