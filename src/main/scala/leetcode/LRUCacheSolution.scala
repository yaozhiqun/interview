package leetcode

import scala.collection.mutable

object LRUCacheSolution extends App {

  case class LRUCache[K, V](size: Int = 4) {

    private var head: Option[Entry] = None

    case class Entry(key: K, value: V, var prev: Option[Entry] = None, var next: Option[Entry] = None) {

      override def toString: String = {
        s"($key - $value) " + next.map(_.toString).getOrElse("")
      }
    }

    private val store = mutable.HashMap[K, Entry]()

    def get(key: K): Option[V] = {
      store.get(key).map(entry => {
        moveToHead(entry) // side effect
        entry.value
      })
    }

    def put(key: K, value: V): LRUCache[K, V] = {
      store.get(key) match {
        case Some(e) =>
          moveToHead(e) // side effect
        case None =>
          if (store.size == size) {
            val last = findLast.getOrElse(throw new IllegalAccessError("There must be a last entry when cache is full"))
            last.prev.foreach(_.next = None)
            store -= last.key
          }
          val newEntry = Entry(key, value, next = head)
          head.foreach(_.prev = Some(newEntry))
          store += key -> newEntry
          moveToHead(newEntry)
      }
      this
    }

    private def moveToHead(entry: Entry): Entry = {
      entry.prev match {
        case Some(p) =>
          // store the prev's prev
          val pp = p.prev
          // store curr's next
          val n = entry.next
          // flip prev and curr
          entry.next = Some(p)
          p.prev = Some(entry)
          // connect prev's prev with curr
          pp.foreach(_.next = Some(entry))
          entry.prev = pp
          // connect prev's next with curr's next
          p.next = n
          n.foreach(_.prev = Some(p))
          // repeat
          moveToHead(entry)
        case None =>
          head = Some(entry)
          entry
      }
    }

    private def findLast: Option[Entry] = {
      def recur(entry: Entry): Entry = {
        entry.next.map(recur).getOrElse(entry)
      }
      head.map(recur)
    }

    override def toString: String = {
      head.map(_.toString).getOrElse("")
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
