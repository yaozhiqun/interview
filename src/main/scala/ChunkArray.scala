object ChunkArray {

  def chunk(array: Array[Any], size: Int): Array[Array[Any]] = {

    def recurse(a: Array[Any], result: Array[Array[Any]]): Array[Array[Any]] = {
      if (a.length < size)
        result ++ Array(a)
      else
        recurse(a.takeRight(a.length - size), result ++ Array(a.take(size)))
    }

    recurse(array, Array[Array[Any]]())
  }

}
