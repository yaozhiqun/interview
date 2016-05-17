import scala.annotation.tailrec

object MiddleNumber extends App {

  def findMiddleIndex(nums: List[Int]): Option[Int] = {
    @tailrec
    def recurse(nums: List[Int], leftSum: Int, index: Int): Option[Int] = {
      nums match {
        case head :: tail =>
          val v = leftSum + head
          val tailSum = tail.sum
          if (v < tailSum) recurse(tail, v, index + 1)
          else if (v == tailSum) Some(index)
          else None
        case Nil => None
      }
    }

    recurse(nums, 0, 0)
  }

  val ints = List(1, 14, 2, 8, 4, 5, 12, 45, 25, 1, 27, 26, 3, 34, 8, 9, 30, 32, 2)
  val mid = findMiddleIndex(ints)
  println(mid)
}
