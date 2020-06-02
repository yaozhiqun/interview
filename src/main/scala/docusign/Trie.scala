package docusign

object Trie extends App {
  case class Trie[V](value: Option[V], children: List[Option[Trie[V]]]) {
    def insert(key: String, value: V): Trie[V] = Trie.insert(this, key, value, 0)
    def delete(key: String): Trie[V] = Trie.delete(this, key, 0)
    def search(key: String): Option[V] = Trie.search(this, key, 0)
  }

  object Trie {
    def empty[V]: Trie[V] = new Trie[V](None, List.fill(26)(None))

    def apply[V]: Trie[V] = empty[V]

    private def search[V](node: Trie[V], key: String, step: Int): Option[V] =
      if (key.length == step) {
        node.value
      } else {
        node.children(key.charAt(step) - 97) match {
          case Some(nextItem) => search(nextItem, key, step + 1)
          case None           => None
        }
      }

    private def insert[V](node: Trie[V], key: String, value: V, step: Int): Trie[V] =
      if (key.length == step) {
        node.copy(value = Some(value))
      } else {
        val index    = key.charAt(step) - 97
        val nextItem = node.children(index).getOrElse(Trie.empty[V])
        val newNode  = insert(nextItem, key, value, step + 1)
        val newNext  = node.children.updated(index, Some(newNode))

        node.copy(children = newNext)
      }

    private def delete[V](node: Trie[V], key: String, step: Int): Trie[V] =
      if (key.length == step) {
        node.copy(value = None)
      } else {
        val index = key.charAt(step) - 97
        node.children(index) match {
          case None           => node
          case Some(nextItem) =>
            val newNode     = delete(nextItem, key, step + 1)
            val newChildren =
              if (newNode.value.isEmpty && newNode.children.forall(_.isEmpty))
                node.children.updated(index, None)
              else
                node.children.updated(index, Some(newNode))

            node.copy(children = newChildren)
        }
      }
  }

  val trie = Trie[Int]
    .insert("to", 7)
    .insert("a", 15)
    .insert("tea", 3)
    .insert("ted", 4)
    .insert("ten", 12)
    .insert("i", 11)
    .insert("in", 5)
    .insert("inn", 9)

  println(trie)

  println(trie.search("ted")) // Some(4)
  println(trie.search("t")) // Some(4)
  println(trie.search("ten")) // Some(12)
  println(trie.search("invalid")) //None

  val newTrie = trie
    .delete("ted")
    .delete("ten")
    .delete("not existing")

  println(newTrie.search("ted")) //None
  println(newTrie.search("ten")) //None
  println(newTrie.search("inn")) //Some(9)
}
