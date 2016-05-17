import scala.util.parsing.combinator.JavaTokenParsers

object Evaluator extends App with ExprParsers {
  val result = parseAll(expr, "1+2*(3+4)-9").map(eval).get
  println(result)
}

trait ExprParsers extends JavaTokenParsers {

  sealed trait Node
  case class Add(left: Node, right: Node) extends Node
  case class Sub(left: Node, right: Node) extends Node
  case class Mul(left: Node, right: Node) extends Node
  case class Div(left: Node, right: Node) extends Node
  case class Num(d: Double) extends Node

  def eval(node: Node): Double = {
    node match {
      case Add(left, right) => eval(left) + eval(right)
      case Sub(left, right) => eval(left) - eval(right)
      case Mul(left, right) => eval(left) * eval(right)
      case Div(left, right) => eval(left) / eval(right)
      case Num(d) => d
    }
  }

  lazy val expr: Parser[Node] = term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (left, "+" ~ right) => Add(left, right)
      case (left, "-" ~ right) => Sub(left, right)
    }
  }

  lazy val term: Parser[Node] = factor ~ rep("[*/]".r ~ factor) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (left, "*" ~ right) => Mul(left, right)
      case (left, "/" ~ right) => Div(left, right)
    }
  }

  lazy val factor: Parser[Node] = "(" ~> expr <~ ")" | num

  lazy val num: Parser[Num] = floatingPointNumber ^^ { n => Num(n.toDouble) }
}