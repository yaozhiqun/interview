object Compress extends App {

  def compress(str: String): String = {

    case class Node(char: Char, count: Int)

    str.toCharArray.foldLeft(List[Node]()) { (nodes, char) =>
      nodes match {
        case head :: tail if head.char == char => head.copy(count = head.count + 1) :: tail
        case _ => Node(char, 1) :: nodes
      }
    }.reverse.map(node => s"${node.char}${node.count}").mkString
  }
}
