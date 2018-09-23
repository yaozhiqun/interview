case class Button(id: Int, var children: List[Button] = Nil, var visited: Boolean = false) {
  def addChild(cs: List[Button]): Unit = {
    children = children ::: cs
  }

  override def toString: String = s"$id"
}

object UnlockPhone extends App {

  def initPhone(): List[Button] = {
    val b1 = Button(1)
    val b2 = Button(2)
    val b3 = Button(3)
    val b4 = Button(4)
    val b5 = Button(5)
    val b6 = Button(6)
    val b7 = Button(7)
    val b8 = Button(8)
    val b9 = Button(9)

    b1.addChild(List(b2, b5, b4))
    b2.addChild(List(b3, b6, b5, b4, b1))
    b3.addChild(List(b6, b5, b2))
    b6.addChild(List(b9, b8, b5, b2, b3))
    b9.addChild(List(b8, b5, b6))
    b8.addChild(List(b7, b4, b5, b6, b9))
    b7.addChild(List(b4, b5, b8))
    b4.addChild(List(b1, b2, b5, b8, b7))
    b5.addChild(List(b1, b2, b3, b6, b9, b8, b7, b4))

    List(
      b1, b2, b3, b4, b5, b6, b7, b8, b9
    )
  }

  def patterns(num: Int): List[List[Button]] = {

    def traverse(button: Button): List[List[Button]] = {

      def rec(parent: Button, children: List[Button], p: List[Button], ps: List[List[Button]] = Nil): List[List[Button]] = {
        children match {
          case head :: tail if head == button =>
            rec(parent, tail, p, ps)
          case head :: tail if head == parent =>
            rec(parent, tail, p, ps)
          case head :: tail if p.contains(head) =>
            rec(parent, tail, p, ps)
          case head :: tail =>
            val currentP = p :+ head
            if (currentP.size == num)
              rec(parent, tail, p, ps :+ currentP)
            else
              rec(head, head.children, currentP, ps)
          case _ =>
            ps
        }
      }

      rec(button, button.children, List(button))
    }

    initPhone().flatMap(traverse)
  }

  println(patterns(5).map(_.mkString("-")).mkString("\n"))
}
