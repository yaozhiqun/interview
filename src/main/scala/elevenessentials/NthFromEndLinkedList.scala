package elevenessentials

object NthFromEndLinkedList extends App {

  case class Node(value: Int, next: Option[Node] = None)

  def nthFromLast(head: Node, n: Int): Option[Int] = {
    val right = (0 until n).toList.foldLeft(head) { case (right, _) =>
      right.next match {
        case Some(node) => node
        case _ => return None
      }
    }

    def rec(left: Node, right: Node): (Node, Node) = {
      right.next match {
        case Some(node) => rec(left.next.get, node)
        case None => (left.next.get, null)
      }
    }
    Some(rec(head, right)._1.value)
  }

  object Node {
    def apply(xs: List[Int]): Node = {
      xs.reverse match {
        case head :: tail =>
          tail.foldLeft(Node(head)) { (node, x) => Node(x, next = Some(node))}
        case Nil => throw new IllegalArgumentException("Can't create linked list from empty list")
      }
    }
  }
  println(nthFromLast(Node(List(5,4,3,2,1)), 2))
  println(nthFromLast(Node(List(5,4,3,2,1)), 6))
}
