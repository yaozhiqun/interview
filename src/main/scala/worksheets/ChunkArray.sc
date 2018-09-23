def chunk(array: Array[Int], size: Int): Array[Array[Int]] = {
  array.zipWithIndex.foldLeft(Array[Array[Int]]()) {
    case (arrays: Array[Array[Int]], (x: Int, index)) =>
      if (index % size == 0)
        arrays :+ Array(x)
      else
        arrays.take(arrays.length - 1) :+ (arrays.takeRight(1).flatten ++ Array(x))
  }
}

chunk(Array(1, 2, 3, 4, 5, 6, 7), 3).deep