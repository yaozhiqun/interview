object LRU extends App {

    case class LRUCache[K, V](size: Int = 8) {
      case class Entry(key: K, value: V, var prev: Option[Entry] = None, var next: Option[Entry] = None) {
        override def toString: String = {
          s"($key - $value) " + next.map(_.toString).getOrElse("")
        }
      }

      private val store = scala.collection.mutable.HashMap[K, Entry]()

      private var head: Option[Entry] = None

      override def toString: String = {
        head.map(_.toString).getOrElse("")
      }

      def get(key: K): Option[V] = {
        store.get(key).map(entry => {
          moveToHead(entry)
          entry.value
        })
      }

      def put(key: K, value: V): Entry = {
        store.get(key) match {
          case Some(e) =>
            moveToHead(e)
          case None =>
            if (store.size == size) {
              val last = findLast.get // TODO
              last.prev.foreach(_.next = None)
              store -= last.key
            }

            val newEntry = Entry(key, value, next = head)
            head.foreach(_.prev = Some(newEntry))
            store += key -> newEntry
            moveToHead(newEntry)

        }
      }

      // (head) e1 - e2 - e3 - e4 - e5 (tail)
      private def moveToHead(entry: Entry): Entry = {
        entry.prev match { // e3
          case Some(p) => // e2
            val pp = p.prev // e1
            val n = entry.next // e4

            // swap e2/e3
            entry.next = Some(p)
            p.prev = Some(entry)

            // connect e1 with e3
            pp.foreach(_.next = Some(entry))
            entry.prev = pp

            // connect e2 with e4
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
        def rec(entry: Entry): Entry = {
          entry.next.map(rec).getOrElse(entry)
        }
        head.map(rec)
      }
    }

    val cache = LRUCache[Int, String](4)
    cache.put(1, "a")
    cache.put(2, "b")
    cache.put(3, "c")
    cache.put(4, "d")
    cache.put(5, "e")

    println(cache)

    println()
    println(cache.get(2))

    println(cache)

    println(cache.get(9))
    println(cache)
}
