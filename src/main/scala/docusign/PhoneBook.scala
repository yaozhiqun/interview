package docusign

import scala.collection.mutable.ListBuffer

object PhoneBook extends App {

  case class Node(value: Char,
                  parent: Option[Node] = None,
                  children: ListBuffer[Node] = ListBuffer[Node](),
                  eow: Boolean = false) {

    def insert(str: String): Node = {
      def recur(xs: List[Char], node: Node): Node = {
        xs match {
          case Nil =>
            node.copy(eow = true)
          case head :: tail =>
              node.children.find(_.value == head)
                .map(recur(tail, _))
                .getOrElse {
                  val newNode = Node(head, parent = Some(node))
                  node.children += newNode.insert(tail.mkString)
                  node
            }
        }
      }
      recur(str.toArray.toList, this)
    }

    def autoComplete(prefix: String): List[String] = {
      def recur(xs: List[Char], nodes: List[Node], foundNode: Option[Node] = None): Option[Node] = {
        xs match {
          case Nil =>
            foundNode
          case head :: tail =>
            nodes.find(_.value == head).flatMap(n => recur(tail, n.children.toList, Some(n)))
        }
      }
      val foundNode = recur(prefix.toArray.toList, children.toList)
      foundNode.map { node =>
        node.values.map(node.parentValue + _)
      }.getOrElse(Nil)
    }

    private def parentValue: String = {
      parent.map(p => p.value.toString.trim + p.parent.map(_.parentValue).getOrElse("")).getOrElse("")
    }

    private def values: List[String] = {
      def recur(parent: Node, children: List[Node], value: String, values: List[String] = Nil): List[String] = {
        children match {
          case Nil =>
            if (parent.eow) value :: values else values
          case head :: tail =>
            recur(head, head.children.toList, value + head.value.toString, values) ::: recur(parent, tail, value, values)
        }
      }
      recur(this, this.children.toList, this.value.toString.trim)
    }

    override def toString: String = {
      values.mkString("\n")
    }
  }

  case class Trie(private val root: Node) {
    def insert(str: String): Trie = {
      root.insert(str)
      this
    }

    def autocomplete(prefix: String): List[String] = {
      root.autoComplete(prefix)
    }

    override def toString: String = {
      root.toString
    }
  }

  object Trie {
    def apply(): Trie = {
      Trie(Node(" ".charAt(0)))
    }
  }
  val trie = Trie().insert("gfor").insert("geek").insert("abc").insert("efxdf").insert("edgfhttx")
  println("Autocomplete for g:")
  trie.autocomplete("g").foreach(println)
  println("Autocomplete for gf:")
  trie.autocomplete("gf").foreach(println)
  println("Autocomplete for e:")
  trie.autocomplete("e").foreach(println)
  println("Autocomplete for ed:")
  trie.autocomplete("ed").foreach(println)
  println("Autocomplete for z:")
  trie.autocomplete("z").foreach(println)
}
