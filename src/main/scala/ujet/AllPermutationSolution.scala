package ujet

object AllPermutationSolution extends App {

  def findAll(sequence: Seq[Char]): List[String] = {
    sequence match {
      case Nil => Nil
      case Seq(head) => List(head.toString)
      case seq =>
        seq.foldLeft(List[String]()) { (l, char) =>
          l ::: findAll(seq.diff(Seq(char))).map(p => char +: p)
        }
//        seq.flatMap(char => findAll(seq.diff(Seq(char))).map(permu => char +: permu)).toList
    }
  }

  println(findAll("abcd"))
  println("abcd".permutations.length)
}
