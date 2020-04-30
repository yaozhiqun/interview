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
}
