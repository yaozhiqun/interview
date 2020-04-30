import java.io.File

import scala.io.Source
import scala.util.Random

object Playground extends App {

  case class Node(value: String, next: Option[Node] = None) {
    override def toString: String = value + next.map("-" + _.toString).getOrElse("")
  }

  case class SingleLinkedList(private val node: Node) {

    def add(value: String): SingleLinkedList = {
      def recur(node: Node): Node = {
        node.copy(next = Some(node.next.map(recur).getOrElse(Node(value))))
      }

      SingleLinkedList(recur(node))
    }

    def reverse: SingleLinkedList = {
      def recur(node: Node, prev: Option[Node] = None): Node = {
        val reversed = node.copy(next = prev)
        node.next.map(recur(_, Some(reversed))).getOrElse(reversed)
      }

      SingleLinkedList(recur(node))
    }

    override def toString: String = {
      node.toString
    }
  }

  object SingleLinkedList {
    def apply(value: String): SingleLinkedList = {
      SingleLinkedList(Node(value))
    }
  }

  val linkedList = SingleLinkedList("a").add("b").add("c").add("d")
  println(linkedList)
  println(linkedList.reverse)
}
