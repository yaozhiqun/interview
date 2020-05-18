import scala.util.Random

/**
 * Created by Zachary Yao on 5/3/16.
 */
object Pi extends App {

<<<<<<< HEAD

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
=======
  def pi(trails: Long): Double = {

    def testCircle: Boolean = {
      val (x, y) = (Random.nextDouble(), Random.nextDouble())
      (x * x + y * y) <= 1
    }

    ((0L until trails).foldLeft(0) { (inCircle, _) =>
      if (testCircle) inCircle + 1 else inCircle
    }.toDouble / trails.toDouble) * 4
  }

  println(pi(100000))
>>>>>>> 6d9129e02e9db8a6f3c34b6e6199bc494669ff8e
}
