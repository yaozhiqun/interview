package rocketlawyer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

class BinNode {
    private final List<BinNode> children = new ArrayList<>();

    BinNode() {}

    BinNode addChild(BinNode child) {
        children.add(child);
        return this;
    }

    List<BinNode> getChildren() {
        return children;
    }

    int height() {
        return recHeight(Collections.singletonList(this), 1);
    }

    private int recHeight(List<BinNode> siblings, int height) {
        List<BinNode> allChildren = siblings.stream()
                .flatMap(sibling -> sibling.getChildren().stream())
                .collect(Collectors.toList());

        return allChildren.isEmpty() ? height : height + recHeight(allChildren, height);
    }
}

public class TreeNodeJ {
    public static void main(String[] args) {
        BinNode tree1 = new BinNode().addChild(new BinNode()).addChild(new BinNode());
        System.out.println(tree1.height());

        BinNode tree2 = new BinNode().addChild(new BinNode()).addChild(new BinNode().addChild(new BinNode()));
        System.out.println(tree2.height());

        BinNode tree3 = new BinNode();
        System.out.println(tree3.height());
    }
}
