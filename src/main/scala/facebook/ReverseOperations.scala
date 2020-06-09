package facebook

object ReverseOperations extends App {

  def solve(xs: List[Int]): List[Int] = {
    case class Node(value: Int, next: Option[Node] = None) {
      def reverse: Node = {
        def recur(node: Node, prev: Option[Node] = None): Node = {
          val reversed = node.copy(next = prev)
          node.next.map(recur(_, Some(reversed))).getOrElse(reversed)
        }
        recur(this)
      }

      def add(value: Int): Node = {
        this.copy(next = Some(Node(value)))
      }

      def toInt: List[Int] = {
        value :: next.map(_.toInt).getOrElse(Nil)
      }
    }

    val (reversed, nodeOpt) = xs.foldLeft((List[Int](), Option.empty[Node])) {
      case ((l, Some(node)), x) if x % 2 == 0 =>(l, Some(node.add(x)))
      case ((l, None), x) if x % 2 == 0 => (l, Some(Node(x)))
      case ((l, Some(node)), x) if x % 2 == 1 => ((l ::: node.reverse.toInt) :+ x, None)
      case ((l, None), x) if x % 2 == 1 => (l :+ x, None)
    }
    reversed ::: nodeOpt.map(_.reverse.toInt).getOrElse(Nil)
  }

  println(solve(List(1, 2, 8, 9, 12, 16)))
  println(solve(List(1, 2, 8, 9, 12, 16, 3)))
}
