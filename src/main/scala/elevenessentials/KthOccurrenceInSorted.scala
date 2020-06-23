package elevenessentials

object KthOccurrenceInSorted extends App {

  def solve(sorted: List[Int], k: Int): Int = {
    val length = sorted.length
    def bs(lo: Int, hi: Int, result: Int = 0): Int = {
      if (lo > hi)
        result
      else {
        val mi = lo + (hi - lo) / 2
        if (sorted(mi) > k)
          bs(lo, mi - 1, result)
        else if (sorted(mi) < k)
          bs(mi + 1, hi, result)
        else {
          val plusOne = result + 1
          if ((mi - 1) >= 0 && sorted(mi - 1) < k)
            bs(mi + 1, hi, plusOne)
          else if ((mi + 1) < (length - 1) && sorted(mi + 1) > k)
            bs(lo, mi - 1, plusOne)
          else
            bs(lo, mi -1, bs(mi + 1, hi, plusOne))
        }
      }
    }
    bs(0, length - 1)
  }

  println(solve(List(1,1,1,2,3,4,5), 1))
  println(solve(List(1,2,2,2,2,2,3,4,5), 2))
}
