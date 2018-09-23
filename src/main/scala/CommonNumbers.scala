object CommonNumbers extends App {

  val l1 = List(1, 3, 4, 6, 7)
  val l2 = List(17, 3, 2, 0, 9)

  val common = l2.foldLeft(List[Int]()) { (l, n) =>
    if (l1.contains(n)) n :: l
    else l
  }

  println(common)
}
