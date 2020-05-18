object Compress extends App {

  def compress(str: String): String = {

    case class Node(char: Char, count: Int) {
      override def toString: String = s"$char$count"
    }

    str.toCharArray.foldLeft(List[Node]()) { (nodes, char) =>
      nodes match {
        case Nil => List(Node(char, 1))
        case init :+ Node(`char`, count) if `char` == char => init :+ Node(char, count + 1)
        case _ => nodes :+ Node(char, 1)
      }
    }.map(_.toString).mkString
  }

  println(compress("aaabbcddddd"))
}
