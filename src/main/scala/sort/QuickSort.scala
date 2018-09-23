package sort

object QuickSort {

  def sort(xs: List[Int]): List[Int] = {
    xs match {
      case head :: _ =>
        val lower = sort(xs.filter(head > _))
        val pivot = xs.filter(head == _)
        val higher = sort(xs.filter(head < _))

        lower ::: pivot ::: higher
      case _ => xs
    }
  }
}
