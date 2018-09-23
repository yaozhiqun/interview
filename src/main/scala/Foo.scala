object Foo extends App {

  def foo(str: String): Map[Char, Int] = {

    def recurse(chars: Array[Char], f: Map[Char, Int]): Map[Char, Int] = {
      chars match {
        case Array(char, tail @_*) =>
          recurse(tail.toArray, f + (char -> f.get(char).map(n => n + 1).getOrElse(1)))
        case Array() =>
          f
      }
    }

    recurse(str.toCharArray, Map[Char, Int]())
  }

  println(foo("hello world"))
}
