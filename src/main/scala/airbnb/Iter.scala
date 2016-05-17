package airbnb

import scala.reflect.ClassTag

class MyList[T](data: Array[T]) {

  case class Iter(var pointer: Int = 0) {

    def hasNext: Boolean = {
      pointer < data.length
    }

    def next(): T = {
      val e = data(pointer)
      pointer = pointer + 1
      e
    }
  }

  def iter: Iter = {
    Iter()
  }
}

object Iter extends App {

  object MyList {
    def apply[T: ClassTag](data: Array[Array[T]]) = {
      val flatten = data.foldLeft(Array[T]()) { case (r1, a1) =>
        r1 ++ a1.foldLeft(Array[T]()) { case (r2, a2) =>
          r2 :+ a2
        }
      }
      new MyList[T](flatten)
//      new MyList[T](data.flatten)
    }
  }

  val list = MyList[Int](Array(Array(), Array(1, 2), Array(3, 4)))
  val iter = list.iter
  while (iter.hasNext)
    print(iter.next())

}
