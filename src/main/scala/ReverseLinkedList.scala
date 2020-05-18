import scala.annotation.tailrec

object ReverseLinkedList extends App {

  case class Node(name: String, next: Option[Node] = None) {

    def add(name: String): Node = {
      val nextNode = next.map(_.add(name)).getOrElse(Node(name))
      copy(next = Some(nextNode))
    }

    override def toString: String = {
      def recurse(node: Node, list: List[String]): List[String] = {
        val current = node.name :: list
        node.next.map(recurse(_, current)).getOrElse(current)
      }
      recurse(this, Nil).reverse.mkString(",")
    }

    def reverse: Node = {
      @tailrec
      def recurse(prev: Option[Node], node: Node): Node = {
        val reversed = node.copy(next = prev)
        node.next match {
          case None => reversed
          case Some(n) => recurse(Some(reversed), n)
        }
      }
      recurse(None, this)
    }
  }

  val node = Node("a").add("b").add("c").add("d").add("e").add("f")
  println(node)
  println(node.reverse)
}
