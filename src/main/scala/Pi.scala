import scala.util.Random

/**
 * Created by Zachary Yao on 5/3/16.
 */
object Pi extends App {

  def pi(trails: Long): Double = {

    def testCircle: Boolean = {
      val (x, y) = (Random.nextDouble(), Random.nextDouble())
      (x * x + y * y) <= 1
    }

    ((0L until trails).foldLeft(0) {
      case (inCircle, _) if testCircle => inCircle + 1
      case (inCircle, _) => inCircle
    }.toDouble / trails.toDouble) * 4
  }

  println(pi(100000))
}
