import com.sun.xml.internal.bind.v2.util.EditDistance

object ShortestPath extends App {

  case class Node(id: Int, var parent: Option[Node] = None, var paths: List[Path] = Nil) {

    def add(path: Path): Unit = {
      paths = paths :+ path
      path.dest.parent = Some(this)
    }

    override def toString: String = {
      s"$id"
    }
  }

  case class Path(orig: Node, dest: Node, distance: Int, var visited: Boolean = false) {
    override def toString: String = {
      s"${orig.id} -$distance-> ${dest.id}"
    }
  }

  val n1 = Node(1)
  val n2 = Node(2)
  val n3 = Node(3)
  val n4 = Node(4)
  val n5 = Node(5)
  val n6 = Node(6)
  val n7 = Node(7)
  val n8 = Node(8)

  val path12 = Path(n1, n2, 1)
  val path13 = Path(n1, n3, 7)
  val path27 = Path(n2, n7, 2)
  val path26 = Path(n2, n6, 3)
  val path34 = Path(n3, n4, 1)
  val path35 = Path(n3, n5, 8)
  val path41 = Path(n4, n1, 5)
  val path65 = Path(n6, n5, 4)
  val path68 = Path(n6, n8, 6)

  n1.add(path12)
  n1.add(path13)
  n2.add(path27)
  n2.add(path26)
  n3.add(path35)
  n3.add(path34)
  n4.add(path41)
  n6.add(path65)
  n6.add(path68)

  def find(orig: Node, dest: Node): List[List[Path]] = {

    def rec(node: Node, paths: List[Path], p: List[Path], ps: List[List[Path]] = Nil): List[List[Path]] = {
      paths match {
        case head :: tail =>
          head.visited = true
          if (head.dest == dest)
            rec(node, tail, p, ps :+ (p :+ head))
          else if (head.dest == orig)
            rec(node, tail, p, ps)
          else
            rec(head.dest, head.dest.paths, p :+ head, ps)
        case Nil =>
          node.parent match {
            case Some(pa) if node != orig =>
              if (p.isEmpty)
                rec(pa, pa.paths.filter(!_.visited), Nil, ps)
              else
                rec(pa, pa.paths.filter(!_.visited), p.reverse.tail.reverse, ps)
            case _ =>
              ps
          }
      }
    }

    rec(orig, orig.paths, Nil)
  }

  def findShortest(orig: Node, dest: Node): Option[List[Path]] = {
    find(orig, dest).sortWith((ps1, ps2) => ps1.map(_.distance).sum < ps2.map(_.distance).sum).headOption
  }

  println(find(n1, n5))
}
