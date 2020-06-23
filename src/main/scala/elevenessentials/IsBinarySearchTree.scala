package elevenessentials

object IsBinarySearchTree extends App {

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None)

  object Node {
    def apply(xs: List[Int]): Node = {
      def rec(lo: Int, hi: Int): Option[Node] = {
        if (lo > hi)
          None
        else {
          val mi = lo + (hi - lo) / 2
          if (xs(mi) == -1)
            None
          else
            Some(Node(xs(mi), rec(lo, mi - 1), rec(mi + 1, hi)))
        }
      }
      rec(0, xs.length - 1).getOrElse(throw new IllegalArgumentException("Cannot create tree with the given list"))
    }
  }

  def isBinarySearchTree(node: Node, lower: Option[Int] = None, upper: Option[Int] = None): Boolean = {
    if (lower.exists(_ > node.value) || upper.exists(_ < node.value))
      false
    else
      node match {
        case Node(value, Some(left), Some(right)) =>
          value > left.value &&
            value < right.value &&
            isBinarySearchTree(left, lower = lower, upper = Some(node.value)) &&
            isBinarySearchTree(right, lower = Some(node.value), upper = upper)
        case Node(value, None, Some(right)) =>
          value < right.value && isBinarySearchTree(right, lower = Some(node.value), upper = upper)
        case Node(value, Some(left), None) =>
          value > left.value && isBinarySearchTree(left, lower = lower, upper = Some(node.value))
        case Node(_, None, None) =>
          true
      }

  }

  println(isBinarySearchTree(Node(List(0,1,2,3,4,5,6)))) // true
  println(isBinarySearchTree(Node(List(0,1,2,3,5,4,6)))) // false
  println(isBinarySearchTree(Node(List(3,1,4,0,5,2,6)))) // false
  println(isBinarySearchTree(Node(List(0,1,4,3,-1,5,-1)))) // false
}
