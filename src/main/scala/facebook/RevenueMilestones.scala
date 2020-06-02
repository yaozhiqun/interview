package facebook

object RevenueMilestones extends App {

  def solve(revenues: List[Int], milestones: List[Int]): List[Int] = {
    revenues.zipWithIndex.foldLeft((List[Int](), milestones, 0)) { case ((hits, miles, sum), (r, i)) =>
      val newSum = sum + r

      if (miles.exists(_ <= newSum))
        (hits :+ (i + 1), miles.filter(_ > newSum), newSum)
      else
        (hits, miles, newSum)
    }._1
  }

  println(solve(List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), List(100, 200, 500)))
}
