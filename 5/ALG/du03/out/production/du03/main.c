#include <stdio.h>
#include <stdlib.h>

struct Node {
    int id;
    struct Node* left;
    struct Node* right;
};

struct MapItem {
    int id;
    struct Node* node;
};

struct Map {
    struct MapItem* items;
    int size;
    int capacity;
};

int maxCities = 0;
int maxCitiesLeft = 0;
int maxCitiesRight = 0;

struct Node* getNode(struct Map* map, int id);
struct Node* buildTree(struct Map* map, int* preorder, int start, int end);
int dfs(struct Node* node);
int depth(struct Node* node);

int main() {
    int n;
    scanf("%d", &n);
    int* preorder = malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        scanf("%d", &preorder[i]);
    }
    
    struct Map map;
    map.items = malloc(n * sizeof(struct MapItem));
    map.size = 0;
    map.capacity = n;
    
    struct Node* root = buildTree(&map, preorder, 0, n - 1);
    dfs(root);
    
    printf("%d\n", maxCities);
    
    free(preorder);
    for (int i = 0; i < map.size; i++) {
        free(map.items[i].node);
    }
    free(map.items);
    
    return 0;
}

struct Node* getNode(struct Map* map, int id) {
    for (int i = 0; i < map->size; i++) {
        if (map->items[i].id == id) {
            return map->items[i].node;
        }
    }
    
    struct Node* node = malloc(sizeof(struct Node));
    node->id = id;
    node->left = NULL;
    node->right = NULL;
    
    struct MapItem item;
    item.id = id;
    item.node = node;
    
    map->items[map->size] = item;
    map->size++;
    
    return node;
}

struct Node* buildTree(struct Map* map, int* preorder, int start, int end) {
    if (start > end) {
        return NULL;
    }
    
    struct Node* root = getNode(map, preorder[start]);
    int i;
    for (i = start + 1; i <= end; i++) {
        if (preorder[i] > root->id) {
            break;
        }
    }
    
    root->left = buildTree(map, preorder, start + 1, i - 1);
    root->right = buildTree(map, preorder, i, end);
    
    return root;
}

int dfs(struct Node* node) {
    if (node == NULL) {
        return 0;
    }
    
    maxCitiesLeft = dfs(node->left);
    int rightCities = 2 + depth(node->right);
    int bestLeft = maxCitiesLeft + rightCities;
    
    maxCitiesRight = dfs(node->right);
    int leftCities = 2 + depth(node->left);
    int bestRight = leftCities + maxCitiesRight;
    
    maxCities = (bestLeft > bestRight) ? bestLeft : bestRight;
    
    return maxCities;
}

int depth(struct Node* node) {
    if (node == NULL) return -1;
    return ((depth(node->left) > depth(node->right)) ? depth(node->left) : depth(node->right)) + 1;
}
