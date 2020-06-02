package facebook

object ReverseToMakeEqual extends App {

  def solve(xs: List[Int], ys: List[Int]): Boolean = {

    def reversed(n: Int, m: Int): Boolean = {
      if (n <= m)
        xs(n) == ys(m) && reversed(n + 1, m - 1)
      else
        true
    }

    if (xs.length != ys.length) {
      false
    } else {
      def traverse(i: Int, lo: Int = -1): Boolean = {
        if (i < xs.length)
          (xs(i) == ys(i), lo) match {
            case (false, -1) =>
              traverse(i + 1, i)
            case (false, lo) =>
              traverse(i + 1, lo)
            case (true, -1) =>
              traverse(i + 1)
            case (true, lo) =>
              if (i < xs.length - 1 && xs(i+1) != ys(i+1)) {
                val hi = 2 * i - lo
                if (reversed(lo, hi)) traverse(hi + 1, -1) else false
              } else if (i < xs.length - 1 && xs(i+1) == ys(i+1)) {
                if (reversed(lo, i - 1)) traverse(i + 1, -1) else false
              } else if (i == xs.length - 1) {
                  (reversed(lo, i - 1))
              } else {
                traverse(i + 1, lo)
              }
          }
        else if (lo != -1)
          reversed(lo, i - 1)
        else
          true
      }
      traverse(0, -1)
    }
  }

  println(solve(List(1,2), List(2,1))) // true
  println(solve(List(1, 2, 3, 4), List(1, 4, 3, 2))) // true
  println(solve(List(1, 2, 3, 4), List(1, 2, 4, 3))) // true
  println(solve(List(1, 2, 3, 4), List(1, 3, 2, 4))) // true
  println(solve(List(1,2,3,4,5,6,7,8,9), List(1,4,3,2,5,6,8,7,9))) // true
  println(solve(List(1,2,3,4,5,6,7,8,9), List(1,2,4,3,5,6,8,7,9))) // true
  println(solve(List(1,4,3,2,5,6,7,8,9), List(1,2,3,4,5,6,8,7,9))) // true
  println(solve(List(1,2,3,4,5,6,7,8,9), List(1,2,4,3,5,6,8,7,1))) // false
  println(solve(List(1,2,3,4,5,6,7,8,9), List(1,2,3,4,5,6,7,8,9).reverse)) // true
}
