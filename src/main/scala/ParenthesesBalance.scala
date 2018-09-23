object ParenthesesBalance extends App {

  def isBalanced(str: String): Boolean = {
    str.toArray.foldLeft(List[Char]()) {
      case (leftPs, lp@'(') =>
        leftPs :+ lp
      case (leftPs, ')') =>
        if (leftPs.nonEmpty)
          leftPs.tail
        else
          return false // messed up
      case (leftPs, _) =>
        leftPs
    }.isEmpty
  }

  println(isBalanced("((dd))"))
  println(isBalanced("((dd))("))
  println(isBalanced(")((dd))()"))
}
