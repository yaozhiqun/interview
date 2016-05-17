object Compress extends App {

  def compress(str: String): String = {

    case class Node(char: Char, count: Int)

    str.toCharArray.foldLeft(List[Node]()) { case (list, char) =>
      list match {
        case head :: tail if head.char == char => head.copy(count = head.count + 1) :: tail
        case _ => Node(char, 1) :: list
      }
    }.reverse.map(node => s"${node.char}${node.count}").mkString
  }

  val str = "1aaaaabcccddd"
  val compressed = compress(str)
  println(compressed)
}
