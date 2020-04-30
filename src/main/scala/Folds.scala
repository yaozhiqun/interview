sealed trait Sex
object Male extends Sex
object Female extends Sex

case class Person(name: String, age: Int, sex: Sex)

// https://coderwall.com/p/4l73-a/scala-fold-foldleft-and-foldright
object Folds extends App {

  def addTitle(persons: Seq[Person]): Seq[String] = {
    persons.foldLeft(Seq[String]()) { (seq, person) =>
      person match {
        case Person(name, age, Male) =>
          seq :+ s"Mr. $name, $age"
        case Person(name, age, Female) =>
          seq :+ s"Ms. $name, $age"
      }
    }
  }

  addTitle(Person("John", 36, Male) :: Person("Jenny", 26, Female) :: Nil)
    .foreach(println)

}
