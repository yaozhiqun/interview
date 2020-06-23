package elevenessentials

object LowestCommonAncestor extends App {
  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None)

  object NodeBuilder {
    def apply(xs: List[Int], miOpt: Option[Int] = None): Node = {
      def rec(lo: Int, hi: Int, miOpt: Option[Int] = None): Option[Node] = {
        if (lo > hi)
          None
        else {
          val mi = miOpt.getOrElse(lo + (hi - lo) / 2)
          if (xs(mi) == -1)
            None
          else
            Some(Node(xs(mi), rec(lo, mi - 1), rec(mi + 1, hi)))
        }
      }
      rec(0, xs.length - 1, miOpt).getOrElse(throw new IllegalArgumentException("Cannot create tree with the given list"))
    }
  }

  def lca(node: Node, x1: Int, x2: Int): Int = {
    def pathToX(n: Node, x: Int, path: List[Int] = Nil): Option[List[Int]] = {
      if (n.value == x) {
        Some(path :+ x)
      } else {
        n match {
          case Node(value, Some(left), Some(right)) =>
            pathToX(left, x, path :+ value) orElse pathToX(right, x, path :+ value)
          case Node(value, Some(left), None) =>
            pathToX(left, x, path :+ value)
          case Node(value, None, Some(right)) =>
            pathToX(right, x, path :+ value)
          case Node(_, None, None) =>
            None
        }
      }
    }
    def findLowest(xs: List[Int], ys: List[Int], last: Int = -1): Int = {
      (xs, ys) match {
        case (x :: xtail, y :: ytail) if x == y => findLowest(xtail, ytail, x)
        case _ => last
      }
    }

    (pathToX(node, x1), pathToX(node, x2)) match {
      case (Some(xs), Some(ys)) => findLowest(xs, ys)
      case _ => -1
    }
  }

  val node = NodeBuilder(List(6,3,7,1,-1,8,-1,5,-1,9,-1,4,-1,2,-1))
  println(lca(node, 7, 8))
  println(lca(node, 2, 2))
  println(lca(node, 8, 9))
  println(lca(node, 1, 5))
}
