object BinarySearch extends App {

  // search the index of the target in the respective array of ints
  // returns the most nearest left index if not found
  def binarySearch(array: Array[Int], target: Int): Int = {

    def search(lo: Int, hi: Int): Int = {
      if (lo >= hi)
        hi
      else {
        val mid = lo + (hi - lo) / 2

        array(mid) match {
          case v if v == target => mid
          case v if v <= target => search(mid + 1, hi)
          case v if v > target => search(lo, mid - 1)
        }
      }
    }
    search(0, array.length - 1)
  }

  val index = binarySearch(Array(1, 2, 3, 4, 6), 10)
  println(index)
}
