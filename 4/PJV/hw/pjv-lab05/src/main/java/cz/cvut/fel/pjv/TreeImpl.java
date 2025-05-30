package cz.cvut.fel.pjv;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

public class TreeImpl implements Tree {

    NodeImpl root;

    public TreeImpl() {
        root = null;
    }

    @Override
    public void setTree(int[] values) {
        root = sortToBinaryTree(values, 0, values.length - 1);
    }

    @Override
    public Node getRoot() {
        return root;
    }

    @Override
    public String toString() {
        return toString(root, "- ");
    }

    private String toString(NodeImpl root, String prefix) {
        if (root == null) {
            return "";
        }
        return prefix + root.data + "\n" + toString(root.left, " " + prefix)
                + toString(root.right, " " + prefix);
    }

    public static NodeImpl sortToBinaryTree(int[] values, int left, int right) {
        if (left > right) {
            return null;
        }
        int mid;
        if ((left + right) % 2 == 0) {
            mid = (left + right) / 2;
        } else {
            mid = (left + right + 1) / 2;
        }
        NodeImpl root = new NodeImpl(values[mid]);
        root.left = sortToBinaryTree(values, left, mid - 1);
        root.right = sortToBinaryTree(values, mid + 1, right);
        return root;
    }
}
