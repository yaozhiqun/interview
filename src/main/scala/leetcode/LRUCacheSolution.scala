package leetcode

import scala.collection.mutable

object LRUCacheSolution extends App {

  case class LRUCache[K, V](size: Int = 4) {

<<<<<<< HEAD
    var first: Option[Entry] = None
    var last: Option[Entry] = None

    case class Entry(key: K, value: V, var prev: Option[Entry] = None, var next: Option[Entry] = None) {

      def bubbleUp(): Entry = {
        def recur(current: Entry): Entry = {
          (current.prev, current.next) match {
            case (Some(p), _) => // has previous entry
              current.prev = p.prev
              p.next = current.next
              p.prev.foreach(_.next = Some(current))
              p.prev = Some(current)
              current.next.foreach(_.prev = Some(p))
              current.next = Some(p)
              recur(current)
            case _ =>
              current
          }
        }

        recur(this)
      }

=======
    private var first: Option[Entry] = None

    case class Entry(key: K, value: V, var prev: Option[Entry] = None, var next: Option[Entry] = None) {

>>>>>>> 6d9129e02e9db8a6f3c34b6e6199bc494669ff8e
      override def toString: String = {
        s"($key - $value) " + next.map(_.toString).getOrElse("")
      }
    }

    private val store = mutable.HashMap[K, Entry]()

    def get(key: K): Option[V] = {
      store.get(key).map(entry => {
<<<<<<< HEAD
        first = Some(entry.bubbleUp()) // side effect
=======
        bubbleUp(entry) // side effect
>>>>>>> 6d9129e02e9db8a6f3c34b6e6199bc494669ff8e
        entry.value
      })
    }

    def put(key: K, value: V): LRUCache[K, V] = {
<<<<<<< HEAD
      val newEntry = store.get(key) match {
        case Some(e) =>
          e.bubbleUp()
=======
      store.get(key) match {
        case Some(e) =>
          bubbleUp(e) // side effect
>>>>>>> 6d9129e02e9db8a6f3c34b6e6199bc494669ff8e
        case None =>
          if (store.size == size) {
            val last = lastOption.getOrElse(throw new IllegalAccessError("Must be a last entry when cache is full"))
            last.prev.foreach(_.next = None)
            store -= last.key
          }
          val newEntry = Entry(key, value, next = first)
          first.foreach(_.prev = Some(newEntry))
          store += key -> newEntry
<<<<<<< HEAD
          newEntry
      }
      first = Some(newEntry)
      this
    }

    def lastOption: Option[Entry] = {
=======
          bubbleUp(newEntry)
      }
      this
    }

    private def bubbleUp(entry: Entry): Entry = {
      entry.prev match {
        case Some(p) =>
          // store the prev's prev
          val pp = p.prev
          // store curr's next
          val nn = entry.next
          // flip prev and curr
          entry.next = Some(p)
          p.prev = Some(entry)
          // connect prev's prev with curr
          pp.foreach(_.next = Some(entry))
          entry.prev = pp
          // connect prev's next with curr's next
          p.next = nn
          nn.foreach(_.prev = Some(p))
          // repeat
          bubbleUp(entry)
        case None =>
          first = Some(entry)
          entry
      }
    }

    private def lastOption: Option[Entry] = {
>>>>>>> 6d9129e02e9db8a6f3c34b6e6199bc494669ff8e
      def recur(entry: Entry): Entry = {
        entry.next.map(recur).getOrElse(entry)
      }
      first.map(recur)
    }

    override def toString: String = {
      first.map(_.toString).getOrElse("")
    }
  }

  val cache = LRUCache[Int, String]().put(1, "a").put(2, "b").put(3, "c").put(4, "d")
  println(cache)
  println(cache.get(3))
  println(cache)
  cache.put(5, "e")
  println(cache.get(2))
  println(cache)
  println(cache.get(5))
  println(cache)
  println(cache.put(6, "z"))
  println(cache.get(9))
}
