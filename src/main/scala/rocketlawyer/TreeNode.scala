package rocketlawyer

object TreeNode extends App {

  case class Node(children: List[Node] = Nil) {
    def height: Int = {
      def rec(nodes: List[Node], level: Int = 1): Int = {
        nodes.flatMap(_.children) match {
          case Nil => level
          case grandChildren => rec(grandChildren, level + 1)
        }
      }
      rec((List(this)))
    }
  }

  val tree1 = Node(List(
                    Node(List(Node())),
                    Node()))
  println((tree1.height)) // 3

  val tree2 = Node(List(
                    Node(List(Node())),
                    Node(List(Node(),
                              Node()))))
  println((tree2.height)) // 3

  val tree3 = Node(List(Node()))
  println((tree3.height)) // 2

  val tree4 = Node()
  println(tree4.height) // 1
}
