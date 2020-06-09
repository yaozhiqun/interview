package rocketlawyer;

import java.util.Optional;

class Node {

    private Node leftChild, rightChild;

    public Node(Node leftChild, Node rightChild) {
        this.leftChild = leftChild;
        this.rightChild = rightChild;
    }

    public Node getLeftChild() {
        return this.leftChild;
    }

    public Node getRightChild() {
        return this.rightChild;
    }

    public int height() {
        return recur(this, 0);
    }

    private int recur(Node node, int height) {
        return Math.max(
                Optional.ofNullable(node.leftChild).map(left -> recur(left, height + 1)).orElse(height),
                Optional.ofNullable(node.rightChild).map(right -> recur(right, height + 1)).orElse(height)
        );
    }

    public static void main(String[] args) {
        Node leaf1 = new Node(null, null);
        Node leaf2 = new Node(null, null);
        Node Node = new Node(leaf1, null);
        Node root = new Node(Node, leaf2);

        System.out.println(root.height());
    }
}