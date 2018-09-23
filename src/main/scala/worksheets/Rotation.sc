
val matrix = Array(Array(1,2), Array(3,4), Array(5,6))
print(matrix)

print(transpose(matrix))
print(rotate90(matrix))
print(rotateMinus90(matrix))

def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] = {
  matrix.head.indices.map(i => matrix.map(_(i))).toArray
}

def rotate90(matrix: Array[Array[Int]]): Array[Array[Int]] = {
  transpose(matrix).map(_.reverse)
}

def rotateMinus90(matrix: Array[Array[Int]]): Array[Array[Int]] = {
  val t = transpose(matrix)
  t.head.indices.foreach { i =>
    (0 until (t.length/2)).foreach { j =>
      val temp = t(j)(i)
      t(j)(i) = t(t.length - j - 1)(i)
      t(t.length - j - 1)(i) = temp
    }
  }
  t
}

def print(matrix: Array[Array[Int]]): Unit = {
  matrix.foreach(r => println(r.mkString(" ")))
  println()
}
