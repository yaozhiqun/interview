package hankcerrank

object SlowestKeySolution extends App {

    case class Stat(encoded: Int, time: Int)

    /*
     * Complete the 'slowestKey' function below.
     *
     * The function is expected to return a CHARACTER.
     * The function accepts 2D_INTEGER_ARRAY keyTimes as parameter.
     */

    def slowestKey(keyTimes: Array[Array[Int]]): Char = {
      val slowest = keyTimes.toList match {
        case head :: tail => tail.foldLeft(List(Stat(head(0), head(1)))) { case (list, array) =>
          Stat(array(0), array(1) - list.last.time) :: list
        }.sortBy(_.time).reverse.map(_.encoded).head
        case _ => throw new IllegalArgumentException("Empty array received")
      }

      ('a' to 'z')(slowest)
    }

    val buttons = Array(Array(0, 2), Array(1, 5), Array(0, 9), Array(2, 15))

    println(slowestKey(buttons))

}
