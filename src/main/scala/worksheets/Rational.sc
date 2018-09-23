case class Rational(molecule: Int, denominator: Int) {
  def +(that: Rational): Rational = {

    def findCommonDenominator: Int = {
      val sorted = Array(this.denominator, that.denominator).sorted
      val (big, small) = (sorted(0), sorted(1))

      if (big % small == 0) {
        big
      } else {
        big * small
      }
    }

    val commonDenominator = findCommonDenominator
    Rational(
      this.molecule * (commonDenominator / this.denominator) + that.molecule * (commonDenominator / that.denominator),
      commonDenominator
    )
  }

  override def toString: String = {
    s"${this.molecule}/${this.denominator}"
  }
}

Rational(1, 5) + Rational(1, 4)
Rational(1, 5) + Rational(3, 5)
