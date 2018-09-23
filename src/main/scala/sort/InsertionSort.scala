package sort

object InsertionSort {

  def sort(xs: List[Int]): List[Int] = {

    def insert(x: Int, xs: List[Int]): List[Int] = {
      xs match {
        case head :: tail if x > head => head :: insert(x, tail)
        case _ => x :: xs
      }
    }

    xs match {
      case head :: tail => insert(head, sort(tail))
      case _ => Nil
    }
  }
}
