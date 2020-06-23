package elevenessentials

object MineFieldExpend extends App {

  def click(arrays: Array[Array[Int]], click: (Int, Int)): Array[Array[Int]] = {
    val maxRow = arrays.length - 1
    val maxCol = arrays(0).length - 1

    case class Dot(x: Int, y: Int) {
      def up: Option[Dot] = if (x - 1 >= 0) Some(Dot(x - 1, y)) else None
      def left: Option[Dot] = if (y - 1 >= 0) Some(Dot(x, y - 1)) else None
      def right: Option[Dot] = if (y + 1 <= maxCol) Some(Dot(x, y + 1)) else None
      def down: Option[Dot] = if (x + 1 <= maxRow) Some(Dot(x + 1, y)) else None
      def upLeft: Option[Dot] = if (x - 1 >= 0 && y - 1 >= 0) Some(Dot(x - 1, y - 1)) else None
      def upRight: Option[Dot] = if (x - 1 >= 0 && y + 1 <= maxCol) Some(Dot(x - 1, y + 1)) else None
      def downLeft: Option[Dot] = if (x + 1 <= maxRow && y - 1 >= 0) Some(Dot(x + 1, y - 1)) else None
      def downRight: Option[Dot] = if (x + 1 <= maxRow && y + 1 <= maxCol) Some(Dot(x + 1, y + 1)) else None

      def allNeighbors: List[Dot] = {
        List(up, left, right, down, upLeft,upRight, downLeft, downRight).flatten
      }
    }

    arrays(click._1)(click._2) match {
      case -1 | 1 => arrays
      case 0 =>
        def rec(dot: Dot, zeroes: List[Dot]): List[Dot] = {
          dot.allNeighbors.foldLeft(zeroes) { (l, d) =>
            if (arrays(d.x)(d.y) == 0 && !l.contains(d))
              rec(d, d :: l)
            else
              l
          }
        }
        val zero = Dot(click._1, click._2)
        val zeroes = rec(zero, List(zero))
        for {
          x <- arrays.indices
          y <- arrays(x).indices
        } {
          if (zeroes.contains(Dot(x, y)))
            arrays(x)(y) = -2
        }
        arrays
    }
  }

  val field1 = Array(
    Array(0, 0, 0, 0, 0),
    Array(0, 1, 1, 1, 0),
    Array(0, 1, -1, 1, 0)
  )

  val field2 = Array(
    Array(-1, 1, 0, 0),
    Array( 1, 1, 0, 0),
    Array( 0, 0, 1, -1)
  )

  println(click(field1, (2, 2)).deep)
  println(click(field1, (1, 4)).deep)

  println(click(field2, (0, 1)).deep)
  println(click(field2, (1, 3)).deep)
}
