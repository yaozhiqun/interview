object BubbleSort extends App {

  def sort(list: List[Int]): List[Int] = {

    def bubble(source: List[Int], result: List[Int]): List[Int] = {
      if (source.isEmpty) result
      else travelOnce(source, Nil, result)
    }

    def travelOnce(source: List[Int], temp: List[Int], result: List[Int]): List[Int]= {
//      println(s"source: [$source] temp: [$temp] result: [$result]")
      source match {
        case h1 :: h2 :: tail =>
          if (h1 > h2) travelOnce(h1 :: tail, h2 :: temp, result)
          else travelOnce(h2 :: tail, h1 :: temp, result)
        case h :: Nil =>
          bubble(temp, h :: result)
      }
    }

    bubble(list, Nil)
  }

  println(sort(List(1,4,2,3,5,7,3,9)))
}
