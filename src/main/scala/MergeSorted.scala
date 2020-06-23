import scala.collection.mutable

object MergeSorted extends App {

  def merge2Sorted(xs: List[Int], ys: List[Int], merged: List[Int] = List[Int]()): List[Int] = {
    (xs, ys) match {
      case (x :: xtail, y :: ytail) =>
        if (x > y) merge2Sorted(xs, ytail, merged :+ y)
        else if (x == y) merge2Sorted(xtail, ytail, merged :+ x :+ y)
        else merge2Sorted(xtail, ys, merged :+ x)
      case (Nil, _) => merged ::: ys
      case (_, Nil) => merged ::: xs
      case (Nil, Nil) => merged
    }
  }

  def mergeSorted(xss: List[List[Int]]): List[Int] = {
    xss match {
      case Nil => Nil
      case head :: Nil => head
      case head :: tail =>
        tail.foldLeft(head) { (l, xs) =>
          merge2Sorted(l, xs)
        }
    }
  }

  println(mergeSorted(List(
    List(1,4,5),
    List(1,3,4),
    List(2,6)
  )))

  case class LinkedNode(x: Int, next: Option[LinkedNode] = None)

  def mergeKLists(nodes: List[LinkedNode]): List[Int] = {
    val pq = mutable.PriorityQueue.empty[LinkedNode](Ordering.by(_.x)).reverse
    nodes.foreach(pq.enqueue(_))

    val result = mutable.ListBuffer[Int]()
    while (pq.nonEmpty) {
      val node = pq.dequeue()
      result += node.x
      node.next.foreach(pq.enqueue(_))
    }
    result.toList
  }

  def mapToLinkNode(xss: List[List[Int]]): List[LinkedNode] = {
    xss.foldLeft(List[LinkedNode]()) { (l, xs) =>
      xs.reverse match {
        case head :: tail => tail.foldLeft(LinkedNode(head)) { (n, x) => LinkedNode(x, next = Some(n)) } :: l
        case Nil => l
      }
    }
  }

  println(mergeKLists(mapToLinkNode(List(
    List(1,4,5),
    List(1,3,4),
    List(2,6)
  ))))
}
