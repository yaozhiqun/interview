package hankcerrank

object SmallestNegativeSolution extends App {


  def smallestNegativeBalance(debts: Array[Array[String]]): Array[String] = {
    val balances = debts.foldLeft(Map[String, Int]()) { (map, array) =>
      val (borrower, lender, amount) = (array(0), array(1), array(2).toInt)
      map +
        (borrower -> map.get(borrower).map(_ - amount).getOrElse(- amount)) +
        (lender -> map.get(lender).map(_ + amount).getOrElse(amount))
    }.filter(_._2 < 0)

    if (balances.nonEmpty) {
      val smallestNegative = balances.toList.map(_._2).sorted.reverse.distinct.head
      balances.filter(_._2 == smallestNegative).keys.toArray
    } else {
      Array("Nobody has negative balance")
    }
  }

  val test = Array(
    Array("Alex", "Blake", "5"),
    Array("Blake", "Alex", "3"),
    Array("Casey", "Alex", "7"))

  println(smallestNegativeBalance(test).deep)
}
