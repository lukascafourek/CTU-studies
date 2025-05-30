#include "bst_tree.h"
#include <atomic>

void bst_tree::insert_recursive(std::atomic<node*>& current, int64_t data) {
    if (current == nullptr) {
        node* new_node = new node(data);
        node* expected = nullptr;
        if (std::atomic_compare_exchange_strong(&current, &expected, new_node)) {
            return;
        }
    }
    if (data < current.load()->data) {
        insert_recursive(current.load()->left, data);
    } else {
        insert_recursive(current.load()->right, data);
    }
}

void bst_tree::insert(int64_t data) {
    insert_recursive(root, data);
}

// Rekurzivni funkce pro pruchod stromu a dealokaci pameti prirazene jednotlivym uzlum
void delete_node(bst_tree::node* node) {
    if (node == nullptr) {
        return;
    }

    delete_node(node->left);
    delete_node(node->right);
    delete node;
}

bst_tree::~bst_tree() {
    delete_node(root);
}
