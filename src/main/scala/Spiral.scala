case class Position(col: Int, row: Int)
case class Element(position: Position, value: Int)

object Spiral {

  def matrix(n: Int): Array[Array[Int]] = {

    val maxRounds = (n + 1) / 2

    def traverse(round: Int, elements: List[Element]): List[Element] = {

      if (round > maxRounds)
        elements
      else {
        val (from, to) = (round, n - round + 1)

        def leftToRight(elements: List[Element]): List[Element] = {
          (from to to).foldLeft(elements) { (es, col) => addElement(col, round, es) }
        }

        def upToBottom(elements: List[Element]): List[Element] = {
          (from to to).foldLeft(elements) { (es, row) => addElement(to, row, es) }
        }

        def rightToLeft(elements: List[Element]): List[Element] = {
          (from to to).foldRight(elements) { (col, es) => addElement(col, to, es) }
        }

        def bottomToTop(elements: List[Element]): List[Element] = {
          (from + 1 until to).foldRight(elements) { (row, es) => addElement(from, row, es) }
        }

        def addElement(col: Int, row: Int, elements: List[Element]): List[Element] = {
          if (elements.exists(e => e.position == Position(col, row)))
            elements
          else
            Element(Position(col, row), elements.headOption.map(e => e.value).getOrElse(0) + 1) :: elements
        }

        bottomToTop(rightToLeft(upToBottom(leftToRight(elements))))
      }
    }

    val elements = (1 to maxRounds).foldLeft(List[Element]()) { (es, round) =>
      traverse(round, es)
    }

    (1 to n).foldLeft(Array[Array[Int]]()) { (array, number) =>
      array :+ elements.filter(e => e.position.row == number).sortBy(_.position.col).map(_.value).toArray
    }
  }
}
