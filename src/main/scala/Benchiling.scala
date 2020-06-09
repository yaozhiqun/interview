object Benchiling extends App {

  def toDna(str: String): List[(Char, Int)] = {
    str.toArray.zipWithIndex.foldLeft(List[(Char, Int)]()) { case (l, (c, index)) =>
      (c, index) :: l
    }.sortBy(_._2)
  }

  def allRotatedDnas(str: String): List[List[(Char, Int)]] = {
    val dna = toDna(str)
    (1 until str.length).foldLeft(List[List[(Char, Int)]](dna)) { (l, i) =>
      l :+ (dna.map { tuple => (tuple._1, (tuple._2 + i) % str.length)}).sortBy(_._2)
    }
  }

//  println(toDna("TGAAA"))
//  println(allRotatedDnas("TGAAA"))

  def uniqueDNASequences(sequences: Array[String]): Int = {
    sequences.foldLeft((List[List[(Char, Int)]](), 0)) { case ((dnses, total), s) =>
      val dna = toDna(s)
      if (dnses.contains(dna))
        (dnses, total)
      else {
//        println(allRotatedDnas(s))
        (allRotatedDnas(s) ::: dnses, total + 1)
      }
    }._2
  }

  println(uniqueDNASequences(Array("TGAAA", "ATGAA", "AATGA")))
  println(uniqueDNASequences(Array("AAA", "TAA", "TAT", "ATA")))

  def dnsMap(str: String): List[(Char, Int)] = {
    str.toArray.foldLeft(Map[Char, Int]()) { case (m, c) =>
      m + (c -> (m.getOrElse(c, 0) + 1))
    }.toList
  }

  def replaceable(s1: String, reference: String): Boolean = {
    val s1Map = dnsMap(s1)
    val refMap = dnsMap(reference)

    refMap.foldLeft(0) { case (replace, (c, n)) =>
      s1Map.find(_._1 == c) match {
        case Some(count) => replace + Math.abs(count._2 - n)
        case _ => replace + n
      }
    } <= 3
  }

  def replace(str: String, reference: String): List[(Char, Int)] = {
    val strMap = dnsMap(str)
    val refMap = dnsMap(reference)

    strMap.map(_._1).foldLeft((List[(Char, Int)](), 0)) {
      case ((l, index), c) =>
        refMap.find(_._1 == c) match {
          case Some(e) => (l ++ (0 until e._2).map(i => (e._1, index + i)), index + e._2)
          case None => (l, index)
        }
    }._1
  }

  def similarDNA(reference: String, candidates: Array[String]): Int = {

    val allRotatedRef = allRotatedDnas(reference)

    candidates.foldLeft(0) { (total, candidate) =>
      if (replaceable(candidate, reference)) {
        val replaced = replace(candidate, reference)
        if (allRotatedRef.contains(replaced)) {
          total + 1
        } else
          total
      } else
        total
    }
  }

  println(similarDNA("AAAAG",Array("AAAAG", "ATTAG", "AGTTT", "GGGGG")))
}
