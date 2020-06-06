package twitch

import scala.collection.mutable.ListBuffer

object MergeIntervals extends App {

  def merge(intervals: List[(Int, Int)], merged: ListBuffer[(Int, Int)] = ListBuffer[(Int, Int)]()): ListBuffer[(Int, Int)] = {
    intervals match {
      case Nil => merged
      case head :: tail =>
        val leftOverlap = merged.find(i => head._1 > i._1 && head._1 < i._2 )
        if (leftOverlap.isDefined) {
          val overlap = leftOverlap.get
          merged -= overlap
          merged += Tuple2(overlap._1, Math.max(overlap._2, head._2))
          return merge(tail, merged)
        }

        val rightOverlap = merged.find(i => head._2 > i._1 && head._2 < i._2)
        if (rightOverlap.isDefined) {
          val overlap = rightOverlap.get
          merged -= overlap
          merged += Tuple2(head._1, Math.max(overlap._2, head._2))
          return merge(tail, merged)
        }

        val includeOverlap = merged.find(i => head._1 <= i._1 && head._2 >= i._2)
        if (includeOverlap.isDefined) {
          val overlap = includeOverlap.get
          merged -= overlap
          merged += Tuple2(head._1, head._2)
          return merge(tail, merged)
        }

        merge(tail, merged += head)
      }
  }

//  println(merge(List((1, 6), (2, 6), (8, 10), (15, 18))))
//  println(merge(List((1, 6), (8, 10), (1, 4), (15, 18))))
//  println(merge(List((3, 6), (1, 4), (8, 10), (15, 18))))
//  println(merge(List((3, 6), (1, 6), (8, 10), (15, 18))))
//  println(merge(List((1, 6), (3, 4), (8, 10), (15, 18))))

  case class Interval(start: Int, var end: Int)
  def merge(intervals: List[Interval]): List[Interval] = {
    val sortedList = intervals.sortWith { _.start < _.start }
    import scala.collection.mutable.ListBuffer
    val res = ListBuffer[Interval]()

    for (i <- sortedList) {
      if (res.nonEmpty && i.start <= res.last.end)
        res.last.end = Math.max(i.end, res.last.end)
      else
        res += i
    }
    res.toList
  }

  println(merge(List(Interval(1, 6), Interval(2, 6), Interval(8, 10), Interval(15, 18))))
  println(merge(List(Interval(1, 6), Interval(1, 4), Interval(8, 10), Interval(15, 18))))
  println(merge(List(Interval(1, 6), Interval(2, 4), Interval(8, 10), Interval(15, 18))))
  println(merge(List(Interval(3, 6), Interval(1, 6), Interval(8, 10), Interval(15, 18))))
//  println(merge(List((1, 6), (8, 10), (1, 4), (15, 18))))
//  println(merge(List((3, 6), (1, 4), (8, 10), (15, 18))))
//  println(merge(List((3, 6), (1, 6), (8, 10), (15, 18))))
//  println(merge(List((1, 6), (3, 4), (8, 10), (15, 18))))
}
