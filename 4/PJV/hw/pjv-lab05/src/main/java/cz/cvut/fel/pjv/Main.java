package cz.cvut.fel.pjv;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

public class Main {

    public static void main(String[] args) {
        int[] values = {1,2,3,4,5};
        TreeImpl root = new TreeImpl();
        root.setTree(values);
        System.out.print(root);
    }
}
//        if (root != null && root.left == null && root.right == null) {
//            return prefix + root.data + "\n";
//        } else if (root != null && root.left == null) {
//            return prefix + root.data + "\n" + toString(root.right, " " + prefix);
//        } else if (root != null && root.right == null) {
//            return prefix + root.data + "\n" + toString(root.left, " " + prefix);
//        } else if (root != null) {
//            return prefix + root.data + "\n" + toString(root.left, " " + prefix)
//                    + toString(root.right, " " + prefix);
//        } else {
//            return "";
//        }