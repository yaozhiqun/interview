package facebook

object BalanceBrackets extends App {

  def solve(str: String): Boolean = {
    val lefts = List('{', '[', '(')
    str.toCharArray.foldLeft(List[Char]()) {
      case (ps, char) if lefts.contains(char) =>
        ps :+ char
      case (ps, '}') if ps.nonEmpty && ps.last == '{' =>
        ps.init
      case (ps, ']') if ps.nonEmpty && ps.last == '[' =>
        ps.init
      case (ps, ')') if ps.nonEmpty && ps.last == '(' =>
        ps.init
      case _ => return false
    }.isEmpty
  }
  println(solve("{[()]}"))
  println(solve("{}()"))
  println(solve("{(})"))
  println(solve(")"))

  def solveWithStack(str: String): Boolean = {
    val lefts = List('{', '[', '(')
    val stack = scala.collection.mutable.Stack[Char]()
    str.toCharArray.foldLeft(stack) {
      case (s, char) if lefts.contains(char) => s.push(char)
      case (s, '}') if s.nonEmpty && s.pop() == '{' => s
      case (s, ']') if s.nonEmpty && s.pop() == '[' => s
      case (s, ')') if s.nonEmpty && s.pop() == '(' => s
      case _ => return false
    }.isEmpty
  }

  println(solveWithStack("{[()]}"))
  println(solveWithStack("{}()"))
  println(solveWithStack("{(})"))
  println(solveWithStack(")"))
}
