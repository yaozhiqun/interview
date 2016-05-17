public class ReverseLinkedListJava {

    static class Node {
        String name;
        Node next;

        Node(String name, Node next) {
            this.name = name;
            this.next = next;
        }

        public String toString() {
            StringBuilder sb = new StringBuilder(name);
            Node temp = next;
            while (temp != null) {
                sb.append(" ");
                sb.append(temp.name);
                temp = temp.next;
            }
            return sb.toString();
        }
    }

    static Node reverse(Node node) {
        return reverse(node, null);
    }

    private static Node reverse(Node node, Node prev) {
        Node reversed = new Node(node.name, prev);
        if (node.next == null)
            return reversed;
        else
            return reverse(node.next, reversed);
    }


    public static void main(String[] args) {
        Node node = new Node("A", new Node("B", new Node("C", null)));
        System.out.println(node);
        Node reversed = reverse(node);
        System.out.println(reversed);
    }
}
