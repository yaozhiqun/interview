object BinarySearch extends App {

  // search the index of the target in the respective list of ints
  // returns the most nearest left index if not found
  def search(xs: List[Int], target: Int): Int = {

    def recur(lo: Int, hi: Int): Int = {
      if (lo >= hi)
          hi
      else {
        val mid = (lo + hi) / 2

        xs(mid) match {
          case v if v == target => mid
          case v if v < target => recur(mid + 1, hi)
          case v if v > target => recur(lo, mid - 1)
        }
      }
    }

    recur(0, xs.size - 1)
  }

  println(search(List(1, 2, 3, 4, 5), 3))
}
