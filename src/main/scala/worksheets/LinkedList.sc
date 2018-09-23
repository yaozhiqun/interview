case class No(value: String, next: Option[No] = None) {
  override def toString: String = {
    value + next.map("-" +  _.toString).getOrElse("")
  }
}

case class LinkedList(node: No) {

  def add(value: String): LinkedList = {

    def rec(current: No): No = {
      current.copy(next = Some(current.next match {
        case None => No(value)
        case Some(n) => rec(n)
      }))
    }

    LinkedList(rec(node))
  }

  def reverse: LinkedList = {

    def rec(current: No, prev: Option[No]): No = {
      val reversed = current.copy(next = prev)
      current.next match {
        case None => reversed
        case Some(next) => rec(next, Some(reversed))
      }
    }

    LinkedList(rec(node, None))
  }

  def sort: LinkedList = {
    fromList(quickSort(toList(this)))
  }

  def exits(value: String): Boolean = {

    val xs = quickSort(toList(this))

    def rec(lo: Int, hi: Int): Boolean = {
      if (lo > hi)
        false
      else {
        val mi = lo + (hi - lo) /2
        if (xs(mi) > value)
          rec(lo, mi - 1)
        else if (xs(mi) == value)
          true
        else
          rec(mi + 1, hi)
      }
    }

    rec(0, xs.length - 1)
  }

  private def toList(list: LinkedList): List[String] = {

    def rec(node: No, xs: List[String] = Nil): List[String] = {
      node.next match {
        case None => xs :+ node.value
        case Some(next) => rec(next, xs :+ node.value)
      }
    }

    rec(list.node)
  }

  private def fromList(xs: List[String]): LinkedList = {

    def rec(xs: List[String], node: Option[No] = None): Option[No] = {
      xs match {
        case head :: tail => Some(No(head).copy(next = rec(tail)))
        case Nil => Option.empty
      }
    }

    LinkedList(rec(xs).getOrElse(throw new IllegalArgumentException()))
  }

  private def quickSort(xs: List[String]): List[String] = {
      xs match {
        case head :: _ =>

          val smaller = quickSort(xs.filter(_ < head))
          val pivot = xs.filter(_ == head)
          val bigger = quickSort(xs.filter(_ > head))

          smaller ::: pivot ::: bigger
        case _ =>
          xs
      }
  }

  override def toString: String = node.toString
}

object LinkedList {
  def apply(value: String): LinkedList = {
    LinkedList(No(value))
  }
}

LinkedList("a").add("b").add("c")
LinkedList("a").add("b").add("c").reverse
LinkedList("a").add("c").add("b").sort
LinkedList("a").add("b").add("c").exits("a")
LinkedList("a").add("b").add("c").exits("b")
LinkedList("a").add("b").add("c").exits("c")
LinkedList("a").add("b").add("c").exits("1")
LinkedList("a").add("c").exits("b")