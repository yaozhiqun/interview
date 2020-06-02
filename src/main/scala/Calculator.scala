object Calculator extends App {

  sealed trait Operate extends Expression {
  }
  case class Add(left: Expression, right: Option[Expression] = None) extends Operate
  case class Sub(left: Expression, right: Option[Expression] = None) extends Operate
  case class Mul(left: Expression, right: Option[Expression] = None) extends Operate
  case class Div(left: Expression, right: Option[Expression] = None) extends Operate

  sealed trait Expression
  case class Num(value: Double) extends Expression

  def calculate(exp: String): Double = {

    def cal(exp: Expression): Double = {
      exp match {
        case Add(left, right) => cal(left) + right.map(cal).getOrElse(throw new IllegalArgumentException("Incomplete operation"))
        case Sub(left, right) => cal(left) - right.map(cal).getOrElse(throw new IllegalArgumentException("Incomplete operation"))
        case Mul(left, right) => cal(left) * right.map(cal).getOrElse(throw new IllegalArgumentException("Incomplete operation"))
        case Div(left, right) => cal(left) / right.map(cal).getOrElse(throw new IllegalArgumentException("Incomplete operation"))
        case Num(n) => n
      }
    }

    def parse: Option[Expression] = {
      val chars = exp.toCharArray.toList
      chars match {
        case Nil => None
        case head :: Nil =>
          Some(Num(head)) // todo
        case head :: tail =>
          val first: Expression = Num(head.toString.toDouble)
          val exp = tail.foldLeft(first) { case (o, c) =>
            c match {
              case '+' => Add(o)
              case '-' => Sub(o)
              case '*' if o.isInstanceOf[Add] => Add(o.asInstanceOf[Add].left, Some(Mul(o.asInstanceOf[Add].right.get)))
              case '/' => Div(o)
              case num if o.isInstanceOf[Add] => o.asInstanceOf[Add].copy(right = Some(Num(num.toString.toDouble)))
              case num if o.isInstanceOf[Sub] => o.asInstanceOf[Sub].copy(right = Some(Num(num.toString.toDouble)))
              case num if o.isInstanceOf[Mul] => o.asInstanceOf[Mul].copy(right = Some(Num(num.toString.toDouble)))
              case num if o.isInstanceOf[Div] => o.asInstanceOf[Div].copy(right = Some(Num(num.toString.toDouble)))
            }
          }
          Some(exp)
      }
    }

    parse.map(cal).getOrElse(throw new IllegalArgumentException("Unable to parse expression."))
  }

  println(calculate("1+2*3"))
}
