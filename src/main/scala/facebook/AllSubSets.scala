package facebook

object AllSubSets extends App {

  def solve(xs: List[Int], ll: List[List[Int]] = List(List())): List[List[Int]] = {
    xs.foldLeft(ll) { case (ll, x) =>
      val newLL = if (ll.contains(xs)) ll else xs :: ll
      solve(xs.diff(List(x)), newLL)
    }
  }

  println(solve(List(1, 2)))
  println(solve(List(1, 2, 3, 4)))
  println(solve(List(1, 2, 3, 4)).size)
}
