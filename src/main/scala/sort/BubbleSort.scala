package sort

import scala.annotation.tailrec

object BubbleSort {

  def sort(list: List[Int], sorted: List[Int] = Nil): List[Int] = {

    @tailrec
    def bubble(bigger: List[Int], smaller: List[Int], sorted: List[Int]): List[Int] = {
      bigger match {
        case h1 :: h2 :: tail =>
          if (h1 > h2) bubble(h1 :: tail, h2 :: smaller, sorted)
          else bubble(h2 :: tail, h1 :: smaller, sorted)
        case biggest :: Nil =>
          sort(smaller, biggest :: sorted)
        case Nil =>
          sort(Nil, sorted)
      }
    }

    list match {
      case Nil => sorted
      case otherwise => bubble(otherwise, Nil, sorted)
    }
  }
}
