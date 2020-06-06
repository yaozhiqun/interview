package rocketlawyer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

class Node {
    private final List<Node> children = new ArrayList<>();

    Node() {}

    Node addChild(Node child) {
        children.add(child);
        return this;
    }

    List<Node> getChildren() {
        return children;
    }

    Integer height() {
        return recHeight(Collections.singletonList(this), 1);
    }

    private Integer recHeight(List<Node> siblings, Integer height) {
        List<Node> allChildren = siblings.stream().flatMap(sibling -> sibling.getChildren().stream()).collect(Collectors.toList());
        return allChildren.isEmpty() ? height : height + recHeight(allChildren, height);
    }
}

public class TreeNodeJ {
    public static void main(String[] args) {
        Node tree1 = new Node().addChild(new Node()).addChild(new Node());
        System.out.println(tree1.height());

        Node tree2 = new Node().addChild(new Node()).addChild(new Node().addChild(new Node()));
        System.out.println(tree2.height());

        Node tree3 = new Node();
        System.out.println(tree3.height());
    }
}
