object QuickSort extends App {

  def quicksort(array: Array[Int]): Array[Int] = {

    if (array.length < 2)
      array
    else {
      val pivot = array(array.length / 2)
      val lo = quicksort(array filter (pivot > _))
      val same = array filter (pivot == _)
      val hi = array filter (pivot < _)

      lo ++ same ++ hi
    }
  }

  val array = Array(12,3,2,2,1,4,4,1)
  println(quicksort(array).mkString(","))

  def quicksort(list: List[Int]): List[Int] = {
    list match {
      case head :: tail =>
        val lower = quicksort(list.filter(head > _))
        val same = list.filter(head == _)
        val higher = quicksort(list.filter(head < _))

        lower ::: same ::: higher
      case _ => list
    }
  }

  val list = List(10,2,3,2,9,4,5,2,1,6)
  println(quicksort(list).mkString(","))

}
