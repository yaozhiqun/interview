package old;

public class MyHashMap<K, V> {

    private int capacity = 8;

    private int locate(K key) {
        return Math.abs(key.hashCode()) % capacity;
    }

    static class Node<K, V> {
        K key;
        V value;
        Node next;

        public Node(K key, V value, Node next) {
            this.key = key;
            this.value = value;
            this.next = next;
        }
    }

    private Node<K, V>[] store;

    public MyHashMap() {
        store = new Node[capacity];
    }

    public V get(K key) {
        int hashCode = locate(key);

        Node<K, V> node = store[hashCode];
        if (node == null) {
            return null;
        } else {
            Node<K, V> tmp = node;
            while (tmp != null) {
                if (tmp.key.equals(key))
                    return tmp.value;

                tmp = tmp.next;
            }
        }

        return null;
    }

    public void put(K key, V value) {
        int hashCode = locate(key);

        Node<K, V> node = store[hashCode];
        if (node == null) {
            store[hashCode] = new Node(key, value, null);
        } else {
            Node current = null;
            Node tmp = node;
            while (tmp != null) {
                if (tmp.key.equals(key)) {
                    current = tmp;
                    tmp.value = value;
                    break;
                }
                tmp = tmp.next;
            }

            if (current == null) {
                store[hashCode] = new Node(key, value, node);
            }
        }
    }

    public void remove(K key) {
        int hashCode = locate(key);

        Node<K, V> node = store[hashCode];
        if (node == null) {
            return;
        } else {
            Node current = null;
            Node tmp = node;
            Node prev = null;
            while (tmp != null) {
                if (tmp.key.equals(key)) {
                    current = tmp;
                    break;
                }
                prev = tmp;
                tmp = tmp.next;
            }

            if (current != null) {
                if (prev != null) {
                    prev.next = current.next;
                } else {
                    store[hashCode] = current.next;
                }
            }
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < capacity; i ++) {
            if (store[i] == null)
                continue;

            Node node = store[i];
            while (node != null) {
                sb.append(node.key + " -> " + node.value);
                sb.append("\n");
                node = node.next;
            }
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        MyHashMap map = new MyHashMap();
        map.put("a", "A");
        map.put("b", "B");
        map.put("a", "D");
        System.out.println(map);
        System.out.println(map.get("a"));
        map.remove("a");
        map.remove("b");
        System.out.println(map);
    }
}
