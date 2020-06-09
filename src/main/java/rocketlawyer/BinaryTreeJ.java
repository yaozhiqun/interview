package rocketlawyer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

class BinTreeNode {
    Optional<BinTreeNode> left = Optional.empty();
    Optional<BinTreeNode> right = Optional.empty();

    BinTreeNode() {}

    BinTreeNode(Optional<BinTreeNode> left, Optional<BinTreeNode> right) {
        this.left = left;
        this.right = right;
    }

    int height() {
        return bfsRec(Collections.singletonList(this), 1);
    }

    int heightDfs() {
        return dfsRec(this, 1);
    }

    private List<BinTreeNode> children() {
        List<BinTreeNode> children = new ArrayList<>();
        left.ifPresent(children::add);
        right.ifPresent(children::add);
        return children;
    }

    private int bfsRec(List<BinTreeNode> siblings, int height) {
        List<BinTreeNode> allChildren = siblings.stream().flatMap(child -> child.children().stream()).collect(Collectors.toList());
        if (allChildren.isEmpty())
            return height;
        else
            return bfsRec(allChildren, height + 1);
    }

    private int dfsRec(BinTreeNode tree, int height) {
        return Math.max(
                tree.left.map(node -> dfsRec(node, height + 1)).orElse(height),
                tree.right.map(node -> dfsRec(node, height + 1)).orElse(height)
        );
    }
}

public class BinaryTreeJ {

    public static void main(String[] args) {
        BinTreeNode tree1 = new BinTreeNode(Optional.of(new BinTreeNode()), Optional.empty());
        System.out.println(tree1.height());
        System.out.println(tree1.heightDfs());

        BinTreeNode tree2 = new BinTreeNode();
        System.out.println(tree2.height());
        System.out.println(tree2.heightDfs());

        BinTreeNode tree3 =
                new BinTreeNode(
                        Optional.of(new BinTreeNode(
                                Optional.of(new BinTreeNode()), Optional.of(new BinTreeNode()))),
                        Optional.empty());
        System.out.println(tree3.height());
        System.out.println(tree3.heightDfs());
    }
}
