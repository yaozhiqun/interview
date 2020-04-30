object ChunkArray extends App {

  def chunk(array: Array[Any], size: Int): Array[Array[Any]] = {

    def recurse(a: Array[Any], result: Array[Array[Any]]): Array[Array[Any]] = {
      if (a.length == 0)
        result
      else if (a.length < size)
        result ++ Array(a)
      else
        recurse(a.takeRight(a.length - size), result ++ Array(a.take(size)))
    }

    recurse(array, Array[Array[Any]]())
  }

  println(chunk(Array(1, 2, 3, 4, 5, 6, 7), 2).deep)
}
