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
    def booleanToDouble: Double = if (test()) 1.0d else 0.0d
    @tailrec
    def recurse(n: Int, sum: Double): Double = {
      n match {
        case 0 => sum / trials
        case x => recurse(n - 1, sum + booleanToDouble)
      }
    }

    recurse(trials, 0.0d)
  }

  val pi = monteCarlo(10000000, circleTest) * 4
  println(pi)
}
