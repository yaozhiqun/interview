object FindTop2Numbers extends App {

  val list = List(1, 2, 10, 55, 100, 3, 2, 1)
  val top12 = list.foldLeft((0, 0)) { case (tuple, int) =>
    val (top1, top2) = tuple
    if (int >= top1) (int, top1)
    else if (int > top2 && int < top1) (top1, int)
    else (top1, top2)
  }

  println(top12)
}
