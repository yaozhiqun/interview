object BinarySearch extends App {

  // search the index of the target in the respective list of ints
  // returns the most nearest left index if not found
  def search(xs: List[Int], x: Int): Int = {

    def recur(lo: Int, hi: Int): Int = {

      if (lo >= hi)
        -1
      else {
        def mi = (lo + hi) / 2

        xs(mi) match {
          case v if v > x => recur(lo, mi)
          case v if v == x => mi
          case v if v < x => recur(mi, hi)
        }
      }
    }

    recur(0, xs.size - 1)
  }

  println(search(List(1, 2, 3, 4, 5), 3))
}
