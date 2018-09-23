import java.lang.reflect.MalformedParametersException

def isBalanced(str: String): Boolean = {
  val m = str.toArray.foldLeft(Map[Char, Int]()) {
    case (map, '(') =>
      map + ('(' -> (map.getOrElse('(', 0) + 1))
    case (map, ')') =>
      map + (')' -> (map.getOrElse(')', 0) + 1))
    case (map, _) =>
      map
  }
  println(m.get('(') )
  println(m.get(')') )
  m.getOrElse('(', 0) == m.getOrElse(')', 0)
}

def isBalanced2(str: String): Boolean = {
  str.toArray.foldLeft(Array[String]()) {
    case (leftPs, lp@'(') =>
      println(lp)
      leftPs :+ lp.toString
    case (leftPs, ')') =>
      println(leftPs)
      if (leftPs.nonEmpty)
        leftPs.tail
      else
        leftPs
//        throw new IllegalArgumentException("")

    case (leftPs, ')') =>
      leftPs
  }.isEmpty
}

//isBalanced("((dd))")
//isBalanced("((dd))(")
//isBalanced(")((dd))()")

isBalanced2("((dd))")
//isBalanced2("((dd))(")
//isBalanced2(")((dd))()")