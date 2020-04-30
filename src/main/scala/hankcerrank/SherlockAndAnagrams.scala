package hankcerrank

import java.util.Scanner

/**
  * https://www.hackerrank.com/challenges/sherlock-and-anagrams/problem
  */

object SherlockAndAnagrams extends App {

  val sc =  new Scanner(System.in)

  def solve(s: String): Long = {
    def recur(s: String, len: Int): Long = {
      val l = (0 to s.length - len).
        map(i => s.substring(i, i + len).sortWith((a, b) => a < b)).
        groupBy(x => x).map(_._2.size).filter(_ > 1).
        map(x => x * (x - 1) / 2).sum

      l
    }

    (1 until s.length).map(l => recur(s, l)).sum
  }

  (0 until sc.nextLine().toInt).
    map(_ => solve(sc.nextLine())).
    foreach(println)
}
