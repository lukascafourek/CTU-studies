package cz.cvut.fel.pjv;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

public class NodeImpl implements Node {

    public int data;
    public NodeImpl left;
    public NodeImpl right;
    public NodeImpl(int data) {
        this.data = data;
        left = null;
        right = null;
    }

    @Override
    public Node getLeft() {
        return left;
    }

    @Override
    public Node getRight() {
        return right;
    }

    @Override
    public int getValue() {
        return this.data;
    }
}
