#include <stdio.h>
#include <stdlib.h>

struct Node {
    int key;
    struct Node* left;
    struct Node* right;
    int depth;
};

struct Node** nodeArray;
int* memo;
int* indexInPreorder;

struct Node* getNode(int key) {
    if (nodeArray[key] == NULL) {
        nodeArray[key] = (struct Node*)malloc(sizeof(struct Node));
        nodeArray[key]->key = key;
        nodeArray[key]->left = NULL;
        nodeArray[key]->right = NULL;
        nodeArray[key]->depth = -1;
    }
    return nodeArray[key];
}

struct Node* buildTreeFromPreorder(int* preorder, int start, int end) {
    if (start > end) {
        return NULL;
    }
    struct Node* root = getNode(preorder[start]);
    int i = start + 1;
    while (i <= end && preorder[i] < root->key) {
        i++;
    }
    root->left = buildTreeFromPreorder(preorder, start + 1, i - 1);
    root->right = buildTreeFromPreorder(preorder, i, end);
    root->depth = 1 + ((root->left != NULL) ? root->left->depth : -1);
    root->depth = (root->right != NULL && root->right->depth > root->depth - 1) ? root->right->depth + 1 : root->depth;
    return root;
}

int depthFirstSearch(struct Node* node) {
    if (node == NULL) {
        return 0;
    }
    if (memo[node->key] != -1) {
        return memo[node->key];
    }
    int maxCitiesLeft = depthFirstSearch(node->left);
    int rightCitiesPlusRoot = 2 + ((node->right != NULL) ? node->right->depth : -1);
    int bestLeft = maxCitiesLeft + rightCitiesPlusRoot;
    int maxCitiesRight = depthFirstSearch(node->right);
    int leftCitiesPlusRoot = 2 + ((node->left != NULL) ? node->left->depth : -1);
    int bestRight = leftCitiesPlusRoot + maxCitiesRight;
    int result = (bestLeft > bestRight) ? bestLeft : bestRight;
    memo[node->key] = result;
    return result;
}

int main(int argc, char **argv) {
    int n;
    scanf("%d", &n);

    nodeArray = (struct Node**)malloc((n + 1) * sizeof(struct Node*));
    memo = (int*)malloc((n + 1) * sizeof(int));
    indexInPreorder = (int*)malloc((n + 1) * sizeof(int));

    int* preorder = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        scanf("%d", &preorder[i]);
    }
    for (int i = 0; i <= n; i++) {
        nodeArray[i] = NULL;
        memo[i] = -1;
    }
    for (int i = 0; i < n; i++) {
        indexInPreorder[preorder[i]] = i;
    }
    struct Node* root = buildTreeFromPreorder(preorder, 0, n - 1);
    printf("%d\n", depthFirstSearch(root));

    free(preorder);
    for (int i = 0; i <= n; i++) {
        if (nodeArray[i] != NULL) {
            free(nodeArray[i]);
        }
    }
    free(nodeArray);
    free(memo);
    free(indexInPreorder);

    return 0;
}
