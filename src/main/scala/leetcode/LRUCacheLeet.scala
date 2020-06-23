package leetcode

object LRUCacheLeet extends App {

  case class LRUCache[K, V](capacity: Int = 4) {
    private val store = scala.collection.mutable.LinkedHashMap[K, V]()

    def get(key: K): Option[V] = {
      store.get(key).map { value =>
        reentry(key, value)
        value
      }
    }

    def put(key: K, value: V): LRUCache[K, V] = {
      store.get(key) match {
        case Some(value) =>
          reentry(key, value)
        case None =>
          if (store.size == capacity) {
            store -= store.head._1
          }
          store += key -> value
      }
      this
    }

    private def reentry(key: K, value: V): Unit = {
      store -= key
      store += key -> value
    }

    override def toString: String = {
      store.toList.mkString(", ")
    }
  }

  val cache = LRUCache[Int, Char]()
  cache.put(1, 'A').put(2, 'B').put(3, 'C').put(4, 'D').put(5, 'E')
  println(cache)

  println(cache.get(4))
  println(cache)

  println(cache.get(9))
  println(cache)
}
