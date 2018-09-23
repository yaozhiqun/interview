def hasNonRepeatedNum(xs: List[String], ns: List[String] = List[String]()): Boolean = {
  xs match {
    case head :: tail =>
      if (head.matches("[0-9.]?") && !ns.filterNot(_ == ".").contains(head))
        hasNonRepeatedNum(tail, head :: ns)
      else {
        false
      }
    case _ =>
      true
  }
}

//hasNonRepeatedNum(List("1", "", "2"))

def isSoduku(matrix: Matrix): Boolean = {

  val transposed = matrix.transpose
  if (matrix.value.size == 9 && transposed.value.size == 9) {
    if (!matrix.value.forall(hasNonRepeatedNum(_)))
      false
    else if (!transposed.value.forall(hasNonRepeatedNum(_)))
      false
    else
      matrix.sliding(3).forall(m => hasNonRepeatedNum(m.value.flatten))
  } else {
    false
  }
}

case class Matrix(value: List[List[String]]) {

  def transpose: Matrix = {
    Matrix(value.head.indices.map(i => value.map(_(i))).toList)
  }

  def sliding(n: Int): List[Matrix] = {

    def vSplit(xs: List[List[String]]): List[Matrix] = {
      xs.flatMap(_.sliding(n, n)).zipWithIndex.foldLeft(Map[Int, List[List[String]]]()) {
        case (map, (list, index)) =>
          val key = index % n
          map + (key -> (map.getOrElse(key, Nil) :+ list))
      }.values.map(Matrix).toList
    }

    value.sliding(n, n).flatMap(vSplit).toList
  }
}


val matrix1 = Matrix(
  List(
    List("5","3",".",".","7",".",".",".","."),
    List("6",".",".","1","9","5",".",".","."),
    List(".","9","8",".",".",".",".","6","."),
    List("8",".",".",".","6",".",".",".","3"),
    List("4",".",".","8",".","3",".",".","1"),
    List("7",".",".",".","2",".",".",".","6"),
    List(".","6",".",".",".",".","2","8","."),
    List(".",".",".","4","1","9",".",".","5"),
    List(".",".",".",".","8",".",".","7","9")
  )
)


isSoduku(matrix1)

//matrix1.split(3).foreach(println)