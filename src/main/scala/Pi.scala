import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by Zachary Yao on 5/3/16.
 */
object Pi extends App {


  def pi(trails: Long): Double = {

    implicit def bool2Int(bool: Boolean): Long = {
      if (bool) 1 else 0
    }

    def testCircle(): Boolean = {
      val (x, y) = (Random.nextDouble(), Random.nextDouble())
      (x * x + y * y) <= 1
    }

    def rec(n: Long, inCircle: Long = 0): Double = {
      if (n == 0) {
        (inCircle.toDouble / trails.toDouble) * 4
      } else {
        rec(n - 1, inCircle + testCircle)
      }
    }

    rec(trails)
  }

  println(pi(100000000))
}
