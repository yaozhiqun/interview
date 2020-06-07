package rocketlawyer

object ArrangeApartments extends App {

  def countArrangements(stories: Int): Int = {

    def rec(n: Int, last: Int = -1, result: Int = 0): Int = {
      if (n > stories)
          result + 1
      else {
        last match {
          case 0 =>
            rec(n + 1, 0, result) + rec(n + 1, 1, result)
          case 1 =>
            rec(n + 1, 0, result)
          case -1 =>
            rec(n + 1, 0, result)
        }
      }
    }
    rec(1)
  }

  println(countArrangements(1))
  println(countArrangements(2))
  println(countArrangements(3))
  println(countArrangements(4))
  println(countArrangements(20))
}
