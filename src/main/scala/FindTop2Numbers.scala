object FindTop2Numbers {

  def find(numbers: List[Int]): (Int, Int) = {
    numbers.foldLeft((0, 0)) { case (tuple, num) =>
      tuple match {
        case (top1, _) if num > top1 => (num, top1)
        case (top1, top2) if num < top2 => (top1, top2)
        case (top1, _) => (top1, num)
      }
    }
  }

}
