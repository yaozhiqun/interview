object SingleLinkedList extends App {

  case class Node(value: String, next: Option[Node] = None) {
    override def toString: String = {
      value + next.map(n => "-" + n.toString).getOrElse("")
    }
  }

  case class SingleLinkedList(private val node: Node) {

    def add(value: String): SingleLinkedList = {

      def recurse(next: Option[Node], prev: Node): Node = {
        prev.copy(next = Some(
          next match {
            case None => Node(value)
            case Some(n) => recurse(n.next, n)
          }
        ))
      }

      SingleLinkedList(recurse(node.next, node))
    }

    def reverse(): SingleLinkedList = {

      def recurse(node: Node, prev: Option[Node]): Node = {
        val reversed = node.copy(next = prev)
        node.next match {
          case None => reversed
          case Some(n) => recurse(n, Some(reversed))
        }
      }
      
      SingleLinkedList(recurse(node, None))
    }

    override def toString: String = {
      node.toString
    }
  }

  object SingleLinkedList {
    def apply(value: String): SingleLinkedList = {
      SingleLinkedList(Node(value))
    }
  }

  val linked = SingleLinkedList("a").add("b").add("c")
  println(s"linked: $linked")
  println(s"reversed: ${linked.reverse()}")
}
