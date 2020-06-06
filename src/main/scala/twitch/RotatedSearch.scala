package twitch

object RotatedSearch extends App {

  def find(xs: List[Int], target: Int): Int = {
    def recur(lo: Int, hi: Int): Int = {
      if (lo > hi)
        -1
      else {
        val mid = lo + (hi - lo) / 2
//        println(s"$lo $mid $hi")
        xs(mid) match {
          case v if v == target => mid
          case v if v > target =>
            if (xs(lo) < xs(mid) && target < xs(lo)) // cannot be here
              recur(mid + 1, hi)
            else
              recur(lo, mid - 1)
          case v if v < target =>
            recur(mid + 1, hi)
        }
      }
    }

    recur(0, xs.length - 1)
  }

  println(find(List(4, 5, 6, 7, 0, 1, 2, 3), 0))
  println(find(List(5, 6, 7, 0, 1, 2, 3, 4), 0))
  println(find(List(7, 0, 1, 2, 3, 4, 5, 6), 0))
  println(find(List(0, 1, 2, 3, 4, 5, 6, 7), 0))
  println(find(List(4,5,6,7,0,1,2), 3))
}
