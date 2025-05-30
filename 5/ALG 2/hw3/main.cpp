#include <iostream>
#include <queue>

using namespace std;

struct Node {
    int color;
    Node* left;
    Node* right;
    Node(const int c, Node* l, Node* r) : color(c), left(l), right(r) {}
};

Node* createTreeFromBFS(const int* numbers, const int size) {
    if (size == 0) {
        return nullptr;
    }
    int idx = 0;
    const auto root = new Node(numbers[idx++], nullptr, nullptr);
    queue<Node*> q;
    q.push(root);
    while (idx < size) {
        if (q.empty()) {
            return nullptr;
        }
        const auto node = q.front();
        q.pop();
        node->left = new Node(numbers[idx++], nullptr, nullptr);
        node->right = new Node(numbers[idx++], nullptr, nullptr);
        if (node->left->color != 2) {
            q.push(node->left);
        }
        if (node->right->color != 2) {
            q.push(node->right);
        }
    }
    return root;
}

int longestLeafToLeaf(const Node* node, const int colorPrev, const int colorCount) {
    if (node == nullptr || colorCount > 2) {
        return 0;
    }
    const auto left = longestLeafToLeaf(node->left, node->color, node->color == colorPrev ? colorCount + 1 : 1);
    const auto right = longestLeafToLeaf(node->right, node->color, node->color == colorPrev ? colorCount + 1 : 1);
    return max(left, right) + 1;
}

int pathThroughNode(const Node* node) {
    if (node == nullptr) {
        return 0;
    }
    const auto left = pathThroughNode(node->left);
    const auto right = pathThroughNode(node->right);
    bool pass = true;
    if (node->left && node->right && node->color == node->left->color && node->color == node->right->color) {
        pass = false;
    }
    if (pass) {
        const auto leftPath = longestLeafToLeaf(node->left, node->color, 1);
        const auto rightPath = longestLeafToLeaf(node->right, node->color, 1);
        return max(max(left, right), leftPath + rightPath + 1);
    }
    return max(left, right);
}

int main() {
    int n;
    cin >> n;
    const auto numbers = new int[n];
    for (int i = 0; i < n; i++) {
        cin >> numbers[i];
    }
    const auto root = createTreeFromBFS(numbers, n);
    const int k = pathThroughNode(root);
    cout << k << endl;
    delete root;
    delete[] numbers;
    return 0;
}
