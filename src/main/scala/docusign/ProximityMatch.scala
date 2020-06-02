package docusign

object ProximityMatch extends App {

  def solve(s: String, p: String): Boolean = {
    val sr = """([a-z])""".r
    val pr = """([?*])""".r

    s.toArray foreach { c =>
      c.toString match {
        case sr(_) => println(s"$c is alphabet")
        case pr(_) => println(s"$c is pattern")
        case _ => println(s"$c not match")
      }
    }
    false
  }

  def solve2(cs: List[Char], ps: List[Char]): Boolean = {
    val sr = """([a-z])""".r
    ps match {
      case Nil => false
      case head :: next :: tail =>
        cs.foldLeft(head) { (p, c) =>
          p.toString match {
//            case "*" if c == next => solve2()
            case sr(_) if c == p => return solve2(cs.tail, tail)
            case _ => return false
          }
        }
    }

    false
  }

//  solve("abc", "124")
//  solve("abc*?\\", "124")

//  println(solve2(List('a'), 'b'))
}

