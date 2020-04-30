package leetcode

import scala.collection.mutable.ArrayBuffer

object HashMapSolution extends App {

  case class Node[String](value: String, next: Option[Node[String]]) {

    def add(value: String): Node[String] = {
      def recur(node: Node[String]): Node[String] = {
        if (node.value == value) {
          node
        } else {
          node.copy(next = Some(next.map(recur).getOrElse(Node(value, None))))
        }

      }
      recur(this)
    }
  }

  case class HashMap() {

    val capacity = 8
    private val store = Array.fill[Option[Node[String]]](capacity) { None }

    private def toHash(key: String): Int = {
      key.hashCode % capacity
    }

    override def toString: String = {
      store.map(_.toString).mkString(", ")
    }

    def put(key: String, value: String): HashMap = {
      val pointer = toHash(key)
      val node = store(pointer)
      store(pointer) = Some(node.map(_.add(value)).getOrElse(Node(value, None)))
      this
    }
//
//    def get(key: String): Option[String] = {
//
//    }
//
//    def remove(key: String): HashMap = {
//
//    }

  }

  println(HashMap().put("a", "b"))
  println(HashMap().put("a", "b").put("a", "b"))
  println(HashMap().put("b", "b"))
}
