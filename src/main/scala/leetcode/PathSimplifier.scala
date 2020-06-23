package leetcode

object PathSimplifier extends App {

  def solve(path: String): String = {
    def rec(path: List[Char], dirs: List[String] = Nil, dir: String = ""): List[String] = {
      path.foldLeft((dirs, dir)) {
        case (("" :: Nil, ".."), '/') => (List(""), "")
//        case ((Nil, ".."), '/') => (List(""), "")
        case ((list, ".."), '/') => (list.init, "")
        case ((list, "."), '/')  => (list, "")
        case ((list, ""), '/')   => (list, "")
        case ((list, str), '/')  => (list :+ str, "")
        case ((list, str), c)    => (list, str :+ c)
      } match {
        case (ps, "..") => ps.init
        case (ps, ".") => ps
        case (ps, "/") => ps
        case (ps, "") => ps
        case (ps, p) => ps :+ p
      }
    }
    val dirs = path.toList match {
      case '/' :: tail =>
        rec(tail, List(""))
      case other =>
        rec(other)
    }

    dirs.mkString("/")
  }

  println(solve("/home/"))
  println(solve("/home//foo/"))
  println(solve("/a/./b/../../c/"))
  println(solve("/a/../../b/../c//.//"))
  println(solve("/a//b////c/d//././/.."))
}
