import scala.util.Random

object Playground extends App {

  case class Node(char: Char, count: Int)

  def compress(str: String): String = {
    str.toCharArray.foldLeft(List[Node]()) { (list, char) =>
      list match {
        case head :: tail if head.char == char => head.copy(count = head.count + 1) :: tail
        case _ => Node(char, 1) :: list
      }
    }.reverse.map(node => s"${node.char}${node.count}").mkString
  }

  println(compress("1aaaaabcccddd"))
}
