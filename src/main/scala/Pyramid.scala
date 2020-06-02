object Pyramid extends App {

  def build(n: Int): String = {

    val middlePoint = n / 2
    val maxRow = middlePoint + 1

    def traverseRow(row: Int, col: Int, built: String): String = {
      (row, col) match {
        case (`maxRow`, _) => built
        case (_, _) => traverseCol(row, col, built)
      }
    }

    def traverseCol(row: Int, col: Int, built: String): String = {
      if (col >= n)
        traverseRow(row + 1, 0, built + "\n")
      else if (col < middlePoint - row || col > middlePoint + row)
        traverseCol(row, col + 1, built + " ")
      else
        traverseCol(row, col + 1, built + "*")
    }

    traverseRow(0, 0, "")

  }

  println(build(3))
}
