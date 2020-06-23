object LogNSolution extends App {

  def logN(number: Double, n: Double = 10): Double = {
    Math.log10(number)/Math.log10(n)
  }

  println(logN(9, 2))
  println(logN(9))

}
