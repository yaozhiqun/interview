package facebook

object EncryptedWords extends App {

  def solve(str: String): String = {

    def recur(lo: Int, hi: Int): String = {
      if (lo > hi) {
        ""
      } else {
        val mi = lo + (hi - lo) / 2
        str.charAt(mi) + recur(lo, mi - 1) + recur(mi + 1, hi)
      }
    }
    recur(0, str.length - 1)
  }

  println(solve("abcd"))
  println(solve("abcxcba"))
  println(solve("facebook"))
}
