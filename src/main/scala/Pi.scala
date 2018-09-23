import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by Zachary Yao on 5/3/16.
 */
object Pi extends App {

  def circleTest(): Boolean = {
    val (x, y) = (Random.nextDouble(), Random.nextDouble())
    Math.sqrt(x * x + y * y) <= 1
  }

  def monteCarlo(trials: Int, test: () => Boolean): Double = {
    def booleanToInt: Int = if (test()) 1 else 0

    @tailrec
    def recurse(n: Int, sum: Int): Double = {
      n match {
        case 0 => sum.toDouble / trials
        case _ => recurse(n - 1, sum + booleanToInt)
      }
    }

    recurse(trials, 0)
  }

  val pi = monteCarlo(100000000, circleTest) * 4

  println(pi)
}
