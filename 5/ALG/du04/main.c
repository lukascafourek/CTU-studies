#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct node_t {
    int id;
    int value;
    bool discovered;
    int distance;
    int colour;
} node_t;

typedef struct queueNode_t {
    node_t *node;
    struct queueNode_t *next;
} queueNode_t;

typedef struct queue_t {
    queueNode_t *head;
    queueNode_t *tail;
} queue_t;

int M, N, C, min_length = 2147483647;
node_t **grids;

queue_t *createQueue(void) {
    queue_t *q = (queue_t*)malloc(sizeof(queue_t));
    if (!q) return NULL;
    q->head = q->tail = NULL;
    return q;
}

void push(queue_t *q, node_t *node) {
    if (!q) return;
    queueNode_t *newNode = (queueNode_t*)malloc(sizeof(queueNode_t));
    if (!newNode) return;
    newNode->node = node;
    newNode->next = NULL;
    if (q->tail)
        q->tail->next = newNode;
    else
        q->head = newNode;
    q->tail = newNode;
}

node_t *pop(queue_t *q) {
    if (!q->head) return NULL;
    queueNode_t *temp = q->head;
    node_t *node = temp->node;
    q->head = q->head->next;
    if (!q->head)
        q->tail = NULL;
    free(temp);
    return node;
}

bool isEmpty(queue_t *q) {
    return q->head == NULL;
}

void bfs(void) {
    int sc = 0;
    queue_t *q = createQueue();
    push(q, &grids[0][M * N - N]);
    while (!isEmpty(q)) {
        node_t *node = pop(q);
        node->discovered = true;
        int neighbours[4], idx = 0;
        if (node->id >= N)
            neighbours[idx++] = node->id - N;
        if (node->id < (M - 1) * N)
            neighbours[idx++] = node->id + N;
        if (node->id % N != 0)
            neighbours[idx++] = node->id - 1;
        if ((node->id + 1) % N != 0)
            neighbours[idx++] = node->id + 1;
        for (int i = 0; i < idx; i++) {
            node_t *n = &grids[node->colour][neighbours[i]];
            if (!n->discovered && n->value <= 0) {
                n->discovered = true;
                n->distance = node->distance + 1;
                if (n->id == grids[sc][N - 1].id) {
                    min_length = n->distance;
                    while (!isEmpty(q))
                        pop(q);
                    free(q);
                    return;
                }
                if (n->value < 0) {
                    sc = abs(n->value);
                    grids[sc][n->id].distance = n->distance;
                    n = &grids[sc][n->id];
                }
                push(q, n);
            }
        }
    }
    free(q);
}

int main(int argc, char **argv) {
    int ret;
    if ((ret = scanf("%d %d %d\n", &M, &N, &C)) != 3) return EXIT_FAILURE;
    grids = (node_t**)malloc((C + 1) * sizeof(node_t*));
    if (!grids)
        return EXIT_FAILURE;
    for (int k = 0; k < C + 1; k++) {
        grids[k] = (node_t*)malloc(M * N * sizeof(node_t));
        if (!grids[k]) {
            for (int i = 0; i < k; i++)
                free(grids[i]);
            free(grids);
            return EXIT_FAILURE;
        }
    }
    for (int k = 0; k < C + 1; k++) {
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < N; j++) {
                node_t *node = &grids[k][i * N + j];
                if (k == 0) {
                    if ((ret = scanf("%d", &node->value)) != 1) return EXIT_FAILURE;
                } else if (grids[0][i * N + j].value == k)
                    node->value = 0;
                else
                    node->value = grids[0][i * N + j].value;
                node->id = i * N + j;
                node->discovered = false;
                node->distance = 0;
                node->colour = k;
            }
        }
    }
    bfs();
    printf("%d\n", min_length);
    for (int k = 0; k < C + 1; k++)
        free(grids[k]);
    free(grids);
    return EXIT_SUCCESS;
}
