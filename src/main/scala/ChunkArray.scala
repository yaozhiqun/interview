object ChunkArray extends App {

  def chunk(array: Array[Any], size: Int): Array[Array[Any]] = {
    array.zipWithIndex.foldLeft(Array[Array[Any]]()) { case (aa, (x, i)) =>
      i % size match {
        case 0 => aa ++ Array(Array(x))
        case _ => aa.init ++ Array(aa.last :+ x)
      }
    }
  }

  println(chunk(Array(1, 2, 3, 4, 5, 6, 7), 2).deep)
}
