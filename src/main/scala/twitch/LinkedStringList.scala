package twitch

case class Node(value: String, next: Option[Node] = None) {

  def add(value: String): Node = {
    def rec(node: Node): Node = {
      node.copy(next = Some(node.next.map(rec).getOrElse(Node(value))))
    }
    rec(this)
  }

  def reverse: Node = {
    def rec(node: Node, prev: Option[Node] = None): Node = {
      val reversed = node.copy(next = prev)
      node.next.map(rec(_, Some(reversed))).getOrElse(reversed)
    }
    rec(this)
  }

  private def nth(n: Int, curr: Int = 1): Option[Node] = {
    if (n < 1) {
      None
    }  else if (curr < n) {
      next.flatMap(_.nth(n, curr + 1))
    } else {
      Some(this)
    }
  }

  def nthFromLast(n: Int): Option[Node] = {
    val nthFromFirst = length - n + 1
    nth(nthFromFirst)
  }

  private def length: Int = {
    1 + next.map(_.length).getOrElse(0)
  }

  override def toString: String = {
    value + next.map(n => " " + n.toString).getOrElse("")
  }
}

object LinkedStringList {

  def fromSentence(str: String): Option[Node] = {
    str.split(" ").toList match {
      case Nil => None
      case head :: tail =>
        Some(tail.foldLeft(Node(head)) { (node, s) => node.add(s) })
    }
  }
}
