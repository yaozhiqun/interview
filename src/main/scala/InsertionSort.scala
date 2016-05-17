object InsertionSort extends App {

  def sort(source: List[Int]): List[Int] = {

    def insert(n: Int, result: List[Int], temp: List[Int]): List[Int] = {
      result match {
        case Nil => n :: temp
        case h :: Nil =>
          if (h >= n) n :: h :: temp
          else h :: n :: temp
        case init :+ t =>
          if (t >= n) insert(n, init, t :: temp)
          else (result :+ n) ::: temp
      }
    }

    source.foldLeft(Nil: List[Int]) { case (result, int) =>
      result match {
        case Nil => List(int)
        case head :: tail => insert(int, result, Nil)
      }
    }
  }

  println(sort(List(1,2,3,2,1,0,8,1,7,3,2)))
}
