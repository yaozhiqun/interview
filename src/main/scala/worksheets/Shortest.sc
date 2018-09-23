case class N(id: Int, var parent: Option[N] = None, var children: List[N] = Nil, var visited: Boolean = false) {

  def add(child: N): Unit = {
    children = children :+ child
    child.parent = Some(this)
  }

  override def toString: String = {
    s"$id"
  }
}

val n1 = N(1)
val n2 = N(2)
val n3 = N(3)
val n4 = N(4)
val n5 = N(5)
val n6 = N(6)
val n7 = N(7)
val n8 = N(8)

n1.add(n2)
n1.add(n3)
n2.add(n7)
n2.add(n6)
n3.add(n5)
//n3.add(n4)
n4.add(n1)
n6.add(n5)
n6.add(n8)

def find(o: N, d: N, ps: List[List[Int]] = Nil): List[List[Int]] = {

  def rec(x: N, xs: List[N], p: List[Int], ps: List[List[Int]] = Nil): List[List[Int]] = {
    xs match {
      case head :: tail =>
        head.visited = true
        if (head == d)
          rec(x, tail, p, ps :+ (p :+ head.id))
        else if (head == o)
          rec(x, tail, p, ps)
        else
          rec(head, head.children, p :+ head.id, ps)
      case Nil =>
        println(x.parent)
        x.parent match {
          case Some(parent) =>
            rec(parent, parent.children.filter(!_.visited), p.take(p.length - 1), ps)
          case _ =>
            ps
        }
    }
  }

  rec(o, o.children, List(o.id))

}

find(n1, n5)