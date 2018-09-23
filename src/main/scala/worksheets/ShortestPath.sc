case class Route(orig: Node, paths: List[Path]) {
  override def toString: String = {
    orig.id + "->" + paths.map(_.dest.id).mkString("->")
  }

  def distance: Int = {
    paths.map(_.distance).sum
  }
}

case class Path(dest: Node, distance: Int) {
  override def toString: String = {
    dest.id + s"[$distance]"
  }
}

case class Node(id: Int, var paths: List[Path] = Nil) {

  def link(dest: Node, distance: Int): Unit = {
    paths = Path(dest, distance) +: paths
  }

  override def toString: String = {
    s"$id"
  }
}

val node0 = Node(0)
val node1 = Node(1)
val node2 = Node(2)
val node3 = Node(3)
val node4 = Node(4)

node0.link(node1, 2)
node0.link(node2, 3)
node1.link(node3, 4)
node1.link(node2, 5)
node2.link(node3, 2)
node3.link(node0, 1)
node4.link(node3, 9)

node0
node1

def find(orig: Node, dest: Node): List[Route] = {

  def searchPath(path: Path, paths: List[Path] = Nil, routes: List[Route] = Nil): List[Route] = {
    val xs = paths :+ path
    if (path.dest == orig)
      Nil
    else if (path.dest == dest)
      routes :+ Route(orig, xs)
    else
      searchNode(path.dest, xs, routes)
  }

  def searchNode(n: Node, paths: List[Path], routes: List[Route]): List[Route] = {
    if (n == dest)
      routes :+ Route(orig, paths)
    else if (n == orig)
      Nil
    else
      n.paths.flatMap(searchPath(_, paths, routes))
  }

  orig.paths.foldLeft(List[Route]()) {
    case (rs, path) => searchPath(path, Nil, rs)
  }

}

find(node0, node3).sortWith((a, b) => a.distance < b.distance)
find(node1, node0).sortWith((a, b) => a.distance < b.distance).headOption
//find(node2, node4).sortWith((a, b) => a.distance < b.distance).headOption

case class R(ps: List[Path])
def find2(orig: Node, dest: Node): List[R] = {

  def rec(ps: List[Path], rs: List[R]): List[R] = {
    ps match {
      case Nil => rs
      case head :: tail =>
        if (head.dest == dest)
          rec(tail, rs)
        else if (head.dest == orig)
          rec(tail, rs)
        else {
          rs.find(_.ps.)
          rec(head.dest.paths, )
        }



    }
  }

  rec(orig.paths.map(_.dest), orig.paths.map(p => R(Nil)))
}