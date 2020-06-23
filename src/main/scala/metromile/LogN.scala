package metromile

object LogN extends App {

  def solve(x: Int, y: Int, threshold: Double = 0.01): Double = {
    def cal(d: Double): Double = {
      (0 until y).foldLeft(1.0) { (r, _) => r * d}
    }
    def rec(lo: Double, hi: Double): Double = {
      val mi = lo + (hi - lo) / 2
      if (hi - lo <= threshold)
        mi
      else {
        val midValue = cal(mi)
        if (midValue > x)
          rec(lo, mi)
        else if (midValue < x)
          rec(mi, hi)
        else
          mi
      }
    }
    rec(1, x)
  }

  println(solve(28, 3))
}
