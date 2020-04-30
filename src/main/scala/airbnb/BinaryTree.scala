package airbnb

/**
 * Created By Zachary Yao
 *
 * Binary tree search in Scala
 */
object BinaryTree extends App {

  case class Node(data: Int, left: Option[Node] = None, right: Option[Node] = None) {

    def flatten: List[Int] = {
      val l: List[Int] = left.map(_.flatten).getOrElse(Nil)
      val r: List[Int] = right.map(_.flatten).getOrElse(Nil)

      (data :: l.reverse).reverse ::: r
    }

    override def toString: String = {
      flatten.mkString(",")
    }
  }

  def insert(node: Option[Node], data: Int): Option[Node] = {
    node match {
      case None => Some(Node(data))
      case Some(n) if data <= n.data => Some(n.copy(left = insert(n.left, data)))
      case Some(n) => Some(n.copy(right = insert(n.right, data)))
    }
  }

  def isPresent(node: Node, value: Int): Boolean = {

    def search(n: Node): Option[Node] = {
      if (value == n.data)
        Some(n)
      else if (value <= n.data)
        n.left.flatMap(search)
      else
        n.right.flatMap(search)
    }

    search(node).isDefined
  }

  val datas = List(20, 10, 15, 5, 30, 10, 40, 35, 50)
  val tree = datas match {
    case  head :: tail => tail.foldLeft(Node(head)) { case (n, d) =>
      insert(Some(n), d).get
    }
  }


//    .foldLeft(None: Option[Node]) { case (node, data) =>
//    insert(node, data)
//  }.get

  println(tree)

  val value = 35
  val result = isPresent(tree, value)
  println(s"$value is present in $datas = $result")
}
