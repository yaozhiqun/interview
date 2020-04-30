object Capitalize extends App {

  def capitalize(str: String): String = {
    val space = " "
    str.split(space).map(word => word.toList match {
      case first :: rest => (first.toUpper :: rest).mkString
    }).mkString(space)
  }

  println(capitalize("hello iterable "))
}
