package twitch

import scala.collection.mutable.ListBuffer

// https://leetcode.com/problems/count-unique-characters-of-all-substrings-of-a-given-string/ 828
object CountAllUniqueCharSubStrings extends App {

  // contr[i] = contr[i-1] - c
  def solve(s: String): Int = {
    s.toArray.zipWithIndex.foldLeft((ListBuffer.fill[Int](128){0}, ListBuffer.fill[Int](128){-1}, 0, 0)) {
      case ((lastPosMap, contrsMap, contr, result), (char, index)) =>
        val preContr = contrsMap(char)
        val curContr = index - lastPosMap(char)
        val newContr = contr - preContr + curContr
        contrsMap(char) = curContr
        lastPosMap(char) = index
        (lastPosMap, contrsMap, newContr, result + newContr)
    }._4
  }

  println(solve("ABC"))
  println(solve("ABA"))
  println(solve("LEETCODE"))
}
