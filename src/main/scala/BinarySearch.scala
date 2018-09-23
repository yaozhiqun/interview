object BinarySearch {

  // search the index of the target in the respective list of ints
  // returns the most nearest left index if not found
  def search(xs: List[Int], target: Int): Int = {

    def search(lo: Int, hi: Int): Int = {
      if (lo >= hi)
          hi
      else {
        val mid = lo + (hi - lo) / 2

        xs(mid) match {
          case v if v == target => mid
          case v if v <= target => search(mid + 1, hi)
          case v if v > target => search(lo, mid - 1)
        }
      }
    }

    search(0, xs.size - 1)
  }

}
