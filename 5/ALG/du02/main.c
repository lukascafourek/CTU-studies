#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

typedef struct node_t {
    int roomNumber;
    int weightSum;
    int transitTime;
    struct node_t *left;
    struct node_t *right;
} node_t;

node_t *nodes[15] = {NULL};

int weightBox[15] = {0};

int bestBalance = INT_MAX, bestTime = INT_MAX;

node_t* createNode(int roomNumber) {
    node_t* newNode = (node_t*)malloc(sizeof(node_t));
    if (!newNode)
        exit(EXIT_FAILURE);
    newNode->roomNumber = roomNumber;
    newNode->weightSum = 0;
    newNode->transitTime = 0;
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}

void freeTree(node_t *node) {
    if (node != NULL) {
        freeTree(node->left);
        freeTree(node->right);
        free(node);
    }
}

int calculateBalance(node_t *node) {
    if (!node) return 0;
    int balance = 0;
    if (node->left) {
        balance += abs(node->weightSum - node->left->weightSum);
        balance += calculateBalance(node->left);
    }
    if (node->right) {
        balance += abs(node->weightSum - node->right->weightSum);
        balance += calculateBalance(node->right);
    }
    return balance;
}

void storeBoxes(node_t *node, int idx, int B, int currentTime) {
    if (!node) return;
    if (idx == B) {
        int currentBalance = calculateBalance(nodes[0]);
        if (currentBalance < bestBalance || (currentBalance == bestBalance && currentTime < bestTime)) {
            bestBalance = currentBalance;
            bestTime = currentTime;
        }
        return;
    }
    currentTime += node->transitTime;
    if (node->weightSum == 0) {
        storeBoxes(node->left, idx, B, currentTime);
        storeBoxes(node->right, idx, B, currentTime);
    }
    if ((node->left == NULL || node->left->weightSum > 0) && (node->right == NULL || node->right->weightSum > 0)) {
        node->weightSum += weightBox[idx];
        storeBoxes(nodes[0], idx + 1, B, currentTime);
        node->weightSum -= weightBox[idx];
    }
}

int main(void) {
    int N, B, r;
    if ((r = scanf("%d %d", &N, &B)) != 2)
        exit(EXIT_FAILURE);
    for (int i = 0; i < B; i++) {
        if ((r = scanf("%d", &weightBox[i])) != 1)
            exit(EXIT_FAILURE);
    }
    int a, b, time;
    for (int i = 0; i < N - 1; i++) {
        if ((r = scanf("%d %d %d", &a, &b, &time)) != 3)
            exit(EXIT_FAILURE);
        if (nodes[a] == NULL) {
            nodes[a] = createNode(a);
        }
        if (nodes[b] == NULL) {
            nodes[b] = createNode(b);
        }
        if (nodes[a]->left == NULL) {
            nodes[a]->left = nodes[b];
        } else if (nodes[a]->right == NULL) {
            nodes[a]->right = nodes[b];
        } else {
            return 1;
        }
        nodes[b]->transitTime = time;
    }
    storeBoxes(nodes[0], 0, B, 0);
    printf("%d %d\n", bestBalance, bestTime);
    freeTree(nodes[0]);
    return EXIT_SUCCESS;
}
