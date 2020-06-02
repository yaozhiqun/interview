package leetcode

object BinaryTree extends App {

  case class Node(value: Int, left: Option[Node] = None, right: Option[Node] = None) {
    def tilt: Int = {
      Math.abs(left.map(_.value).getOrElse(0) - right.map(_.value).getOrElse(0))
    }

    def univalued: Boolean = {
      (left, right) match  {
        case (Some(l@Node(this.value, _, _)), Some(r@Node(this.value, _, _))) =>
          l.univalued && r.univalued
        case (Some(l@Node(this.value, _, _)), None) =>
          l.univalued
        case (None, Some(r@Node(this.value, _, _))) =>
          r.univalued
        case (None, None) =>
          true
        case _ =>
          false
      }
    }

    def numOfVisibleLeftmost: Int = {
      def dfsCount(node: Node, sum: Int = 0): Int = {
        val sumLeft = node.left.map(l => {
          dfsCount(l, sum + 1)
        }).getOrElse(sum)
        val sumRight = node.right.map(r => {
          dfsCount(r, sumLeft)
        }).getOrElse(sumLeft)
        sumRight
      }
//      dfsCount(this)
      def bfsCount(nodes: List[Node], sum: Int = 0): Int = {
        nodes match {
          case Nil =>
            sum
          case _ =>
            bfsCount(nodes.flatMap(_.left) ::: nodes.flatMap(_.right), nodes.count(_.left.isDefined) + sum)
        }

      }
      bfsCount(List(this))
    }
  }

  val tree1 = Node(1,
                    Some(Node(2)),
                    Some(Node(3)))
  println(tree1.tilt)
  println(tree1.univalued)

  val tree2 = Node(1,
                    Some(Node(1,
                              Some(Node(1)), Some(Node(1)))),
                    Some(Node(1,
                              Some(Node(1)))))

  println(tree2.univalued)

  def fromArray(array: Array[Int]): Option[Node] = {
    def recur(lo: Int, hi: Int): Option[Node] = {
      if (lo > hi)
        None
      else {
        val mi = lo + (hi - lo) / 2
        Some(Node(array(mi), left = recur(lo, mi - 1), right = recur(mi + 1, hi)))
      }
    }
    recur(0, array.length - 1)
  }

  println(fromArray(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)))

  val visibleLeftmost = Node(8,
                             Some(Node(3,
                               Some(Node(1)),
                               Some(Node(6,
                                 Some(Node(4)),
                                 Some(Node(7)))))),
                             Some(Node(10,
                                None,
                               Some(Node(14,
                                 Some(Node(13))))))
  )
  println(visibleLeftmost.numOfVisibleLeftmost)
}
