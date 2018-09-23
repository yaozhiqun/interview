case class Vertex(id: Int, children: List[Vertex] = Nil) {
  def add(child: Vertex): Vertex = {
    this.copy(children = child :: children)
  }
}

val vertex = Vertex(1)
  .add(Vertex(2)
    .add(Vertex(6))
    .add(Vertex(3)
      .add(Vertex(4))
      .add(Vertex(5))))
  .add(Vertex(7))
  .add(Vertex(8)
    .add(Vertex(12))
    .add(Vertex(9)
      .add(Vertex(10))
      .add(Vertex(11))))

def dfs(vertex: Vertex, order: List[Int] = Nil): List[Int] = {
  vertex.children match {
    case Nil => vertex.id :: order
    case _ => vertex.id :: vertex.children.flatMap(dfs(_))
  }
}

def bfs(vertex: Vertex): List[Int] = {

  def rec(children: List[Vertex], order: List[Int]): List[Int] = {
    children match {
      case Nil => order
      case _ => rec(children.flatMap(_.children), order ::: children.map(_.id))
    }
  }

  rec(vertex.children, vertex.id :: Nil)
}

dfs(vertex)
bfs(vertex)
