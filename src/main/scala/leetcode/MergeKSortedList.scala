package leetcode

class MergeKSortedList extends App {

  def solve(xss: List[List[Int]]): List[Int] = {
    def fetchSmallest(xs: List[Int]): List[Int] = {
      ???
    }
    def rec(yss: List[List[Int]], merged: List[Int] = Nil): List[Int] = {
      yss.flatMap(_.headOption)
    }
    rec(xss)
  }


}
