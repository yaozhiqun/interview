package leetcode

object Vertex extends App {

  case class Vertex(value: Int, children: List[Vertex] = Nil) {
    def add(child: Vertex): Vertex = {
      this.copy(children = children :+ child)
    }

    // Depth First Search
    // Idea: recur with children and return whichever matches first
    def dfs(target: Int): Boolean = {
      found(target) || children.exists(_.dfs(target))
    }

    // Breadth First Search
    // Idea: introduce siblings (or neighbors), recur by generation, children and children's children
    def bfs(target: Int, siblings: List[Vertex] = Nil): Boolean = {
      found(target) || siblings.exists(_.found(target)) || {
        children ::: siblings.flatMap(_.children) match {
          case Nil => false
          case child :: sib => child.bfs(target, sib)
        }
      }
    }

    // Given the target, find the shortest path
    def findShortestPath(target: Int): List[Int] = {

      def recur(vertex: Vertex = this, path: List[Int] = Nil, paths: List[List[Int]] = Nil): List[List[Int]] = {
        if (vertex.found(target)) {
          (path :+ vertex.value) :: paths
        } else {
          vertex.children.flatMap(child => recur(child, path :+ vertex.value, paths))
        }
      }

      def paths = recur()
      // debug
      // paths.foreach(println)
      paths.sortBy(_.length).headOption.getOrElse(Nil)
    }

    private def found(target: Int): Boolean = {
      // debug
      //println("Search on " + value)
      value == target
    }

    // Depth First Ordering
    // Idea: recur with children
    def dfOrder: List[Int] = {
      value :: children.flatMap(_.dfOrder)
    }

    // Breadth First Ordering
    // Idea: recur by generation, children and children's children
    def bfOrder: List[Int] = {

      def recur(vertices: List[Vertex], orders: List[Int]): List[Int] = {
        vertices match {
          case Nil => orders
          case _ => recur(vertices.flatMap(_.children), orders ::: vertices.map(_.value))
        }
      }

      recur(children, value :: Nil)
    }
  }

  /**
    *        1
    *    /  |  \  \
    *   2   |  7  8
    *  / \  |    / \
    * 6  3  |   12  9
    *   / \ |      / \
    *  4   5      10  11
    */
  val v5 = Vertex(5)
  val vertex = Vertex(1)
    .add(Vertex(2)
      .add(Vertex(6))
      .add(Vertex(3)
        .add(Vertex(4))
        .add(v5)))
    .add(v5)
    .add(Vertex(7))
    .add(Vertex(8)
      .add(Vertex(12))
      .add(Vertex(9)
        .add(Vertex(10))
        .add(Vertex(11))))

//  println(vertex.dfs(12))
//  println(vertex.bfs(5))
  println(vertex.dfOrder)
  println(vertex.bfOrder)

//  println(vertex.findShortestPath(5))
//  println(vertex.findShortestPath(18))
}
