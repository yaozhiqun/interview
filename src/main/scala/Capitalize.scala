object Capitalize {

  def capitalize(str: String): String = {
    val space = " "
    str.split(space).map(word => word.toArray.toList match {
      case first :: rest => (first.toUpper :: rest).mkString
    }).mkString(space)
  }
}
