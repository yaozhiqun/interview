object SortLinkedList extends App {

  case class Node(value: Int, next: Option[Node] = None) {

    def add(v: Int): Node = {
      val nextNode = next.map(_.add(v)).getOrElse(Node(v))
      copy(next = Some(nextNode))
    }

    def sort = {
      fromList(quickSort(toList))
    }

    def exists(value: Int): Boolean = {
      def recurse(node: Option[Node]): Boolean = {
        node match {
          case Some(n) =>
            if (n.value == value) true
            else recurse(n.next)
          case _ => false
        }
      }
      recurse(Some(this))
    }

    def bExists(value: Int): Boolean = {
      val sorted = quickSort(toList)

      def recurse(lo: Int, hi: Int): Boolean = {
        if (lo >= hi)
          false
        else {
          val mid = lo + (hi - lo) / 2
          if (sorted(mid) == value)
            true
          else if (sorted(mid) > value)
            recurse(lo, mid - 1)
          else
            recurse(mid + 1, hi)
        }
      }
      recurse(0, sorted.length)
    }

    override def toString = {
      toList.mkString(",")
    }

    private def toList: List[Int] = {
      def recurse(node: Node, list: List[Int]): List[Int] = {
        val currentList = list :+ node.value
        node.next.map(next => recurse(next, currentList)).getOrElse(currentList)
      }
      recurse(this, Nil)
    }

    private def fromList(list: List[Int]): Node = {
      list match {
        case head :: tail => tail.foldLeft(Node(head)) { case (n, int) => n.add(int) }
        case _ => throw new IllegalArgumentException("Could not construct a linked list from an empty list")
      }
    }

    private def quickSort(list: List[Int]): List[Int] = {
      if (list.isEmpty)
        list
      else {
        val pivot = list(list.length / 2)
        val lo = quickSort(list.filter(_ < pivot))
        val hi = quickSort(list.filter(_ > pivot))
        val same = list.filter(_ == pivot)

        lo ::: same ::: hi
      }
    }
  }

  val linkedList = Node(1).add(7).add(5).add(3).add(2)
  println(linkedList)
  println(linkedList.sort)
  println(linkedList.exists(7))
  println(linkedList.bExists(10))
}

