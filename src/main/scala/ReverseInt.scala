/**
 * Created by Zachary Yao on 5/12/16.
 */
object ReverseInt extends App {

  def reverse(n: Int): Int = {

    def recurse(n: Int, acc: Int): Int = {
      val x = n / 10
      val y = n % 10
      x match {
        case 0 => x + acc
        case _ => recurse(x, acc * 10 + y)
      }
    }

    recurse(n, 0)
  }

  println(reverse(123))
}
