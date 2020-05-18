object Rational extends App {

  case class Rational(numer: Int, denom: Int) {

    def +(that: Rational): Rational = {
      val n = numer * that.denom + denom * that.numer
      val d = denom * that.denom
      Rational(n, d)
    }

    override def toString: String = {

      def lcd(a: Int, b: Int): Int = {
        if (b == 0)
          a
        else
          lcd(b, a % b)
      }

      val r = lcd(numer, denom)

      s"${numer/r}/${denom/r}"
    }
  }

  println(Rational(1, 5) + Rational(3, 5))

}
