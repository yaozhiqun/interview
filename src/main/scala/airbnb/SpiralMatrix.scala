package airbnb

import scala.collection.mutable.ListBuffer

/**
 * Created by Zachary Yao
 */
object SpiralMatrix extends App {

  def spiral(matrix: Seq[Seq[Any]], lb: ListBuffer[Any]): Unit = matrix match {
    case head :: tail => 
      head.foreach(lb += _) // adding 1, 2, 3
      tail.foreach(lb += _.last) // adding 6, 9
      tail match {
        case init :+ last =>
          last.init.reverse.foreach(lb += _) // adding 8, 7
          init.reverse.foreach(lb += _.head) // adding 4
          spiral(
            init.map {
              case _ :: (trimmedRow :+ _) => trimmedRow // next spiral matrix
              case _ => Nil
          }.filter(_ != Nil), lb)
        case _ =>
      }
    case _ =>
  }

  override def main(args: Array[String]) {

    def printSpiral(matrix: Seq[Seq[Any]]): Unit = {
      if (matrix.isEmpty || matrix.map(_.length).distinct.size > 1)
        println("Input is not a matrix")
      else {
        println("matrix:")
        matrix.foreach(row => println(row.mkString(",")))
        val lb = ListBuffer.empty[Any]
        spiral(matrix, lb)
        print("spiral:")
        println()
        lb.mkString(",").foreach(print)
        println("\n")
      }
    }

    val input = ListBuffer.empty[Seq[_]]
    for (ln <- io.Source.stdin.getLines()) {
      if (ln == "") {
        printSpiral(input.toSeq)
        System.exit(0)
      } else {
        input += ln.toCharArray
      }
    }
  }
}
