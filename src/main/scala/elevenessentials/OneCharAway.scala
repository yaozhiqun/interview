package elevenessentials

object OneCharAway extends App {

  def solve(a: String, b: String): Boolean = {
    def sameLength(xs: List[Char], ys: List[Char], result: Int = 0): Boolean = {
        (xs, ys) match {
          case (x :: xtail, y :: ytail) =>
            if (x == y)
              sameLength(xtail, ytail, result)
            else
              if (result == 1)
                return false
              else {
                sameLength(xtail, ys, result + 1)
              }
              sameLength(xtail, ytail, x + 1)
          case (Nil, Nil) =>
            true

      }
    }

    def oneCharAway(long: List[Char], short: List[Char], result: Int = 0): Boolean = {
      (long, short) match {
        case (x :: xtail, y :: ytail) =>
          if (x == y)
            oneCharAway(xtail, ytail, result)
          else {
            if (result == 1)
              false
            else {
              oneCharAway(xtail, short, result + 1)
            }
          }
        case (_ :: Nil, Nil) if result == 1 =>
          false
        case _ =>
          true
      }
    }

    if (Math.abs(a.length - b.length) > 1)
      false
    else if (a.length == b.length)
      sameLength(a.toList, b.toList)
    else if (a.length > b.length)
      oneCharAway(a.toList, b.toList)
    else
      oneCharAway(b.toList, a.toList)
  }

  println(solve("abcde", "abcd"))
  println(solve("aa", "abc"))
}
