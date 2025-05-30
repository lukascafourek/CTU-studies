package alg;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;

public class Main {

    static class Node {
        int key;
        Node left;
        Node right;
        int depth;

        public Node(int key) {
            this.key = key;
            this.left = null;
            this.right = null;
            this.depth = -1;
        }
    }

    static Node[] nodeArray;
    static int[] memo;
    static int[] indexInPreorder;

    public static void main(String[] args) throws Exception {
        BufferedReader buffer = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(buffer.readLine().trim());
        String[] nodes = buffer.readLine().trim().split(" ");
        int[] preorder = new int[n];
        for (int i = 0; i < n; i++) {
            preorder[i] = Integer.parseInt(nodes[i]);
        }
        nodeArray = new Node[n + 1];
        memo = new int[n + 1];
        Arrays.fill(memo, -1);
        indexInPreorder = new int[n + 1];
        for (int i = 0; i < n; i++) {
            indexInPreorder[preorder[i]] = i;
        }
        Node root = buildTreeFromPreorder(preorder, 0, n - 1);
        System.out.println(depthFirstSearch(root));
    }

    static Node buildTreeFromPreorder(int[] preorder, int start, int end) {
        if (start > end) {
            return null;
        }
        Node root = getNode(preorder[start]);
        int i = start + 1;
        while (i <= end && preorder[i] < root.key) {
            i++;
        }
        root.left = buildTreeFromPreorder(preorder, start + 1, i - 1);
        root.right = buildTreeFromPreorder(preorder, i, end);
        root.depth = 1 + Math.max(root.left != null ? root.left.depth : -1, root.right != null ? root.right.depth : -1);
        return root;
    }

    static Node getNode(int key) {
        if (nodeArray[key] == null) {
            nodeArray[key] = new Node(key);
        }
        return nodeArray[key];
    }

    static int depthFirstSearch(Node node) {
        if (node == null) {
            return 0;
        }
        if (memo[node.key] != -1) {
            return memo[node.key];
        }
        int maxCitiesLeft = depthFirstSearch(node.left);
        int rightCitiesPlusRoot = 2 + (node.right != null ? node.right.depth : -1);
        int bestLeft = maxCitiesLeft + rightCitiesPlusRoot;
        int maxCitiesRight = depthFirstSearch(node.right);
        int leftCitiesPlusRoot = 2 + (node.left != null ? node.left.depth : -1);
        int bestRight = leftCitiesPlusRoot + maxCitiesRight;
        int result = Math.max(bestLeft, bestRight);
        memo[node.key] = result;
        return result;
    }
}
