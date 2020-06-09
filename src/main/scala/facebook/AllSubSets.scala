package facebook

object AllSubSets extends App {

  def solve(xs: List[Int], ll: List[List[Int]] = List(List())): List[List[Int]] = {
    xs.zipWithIndex.foldLeft(ll) { case (ll, (x, i)) =>
      val withX = xs.take(i) ::: (x :: xs.takeRight(xs.length - i - 1))
      val newLL = if (ll.contains(withX)) ll else withX :: ll
      solve(xs.diff(List(x)), newLL)
    }
  }

  println(solve(List(1, 2)))
  println(solve(List(1, 2, 3, 4)))
}
