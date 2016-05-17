object ReverseString extends App {

  def reverse(str: String): String = {
    str.toCharArray.foldLeft(Array[Char]()) { (array, char) =>
      char +: array
    }.mkString
  }

  println(reverse("hello world"))
}
