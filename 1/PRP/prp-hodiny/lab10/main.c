#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define QUEUE_DEFAULT_SIZE 10

typedef struct Queue {
  int *data;
  size_t head;
  size_t tail;
  size_t size;
} Queue;

Queue *create_queue(size_t size) {
    Queue* q = (Queue*)malloc(sizeof(Queue));
    if (!q)
        return NULL;
    q->data = (int*)malloc(sizeof(int) * size);
    if (!q->data) {
        free(q);
        return NULL;
    }
    q->head = 0;
    q->tail = 0;
    q->size = size;
    return q;
}

void input_queue(Queue *qp) {
    size_t new_size = qp->tail - qp->head + QUEUE_DEFAULT_SIZE;
    int* new_data = (int*)malloc(sizeof(int) * new_size);
    for (int i = qp->head; i < qp->tail; ++i)
        new_data[i - qp->head] = qp->data[i];
    free(qp->data);
    qp->data = new_data;
    qp->size = new_size;
    qp->tail = qp->tail - qp->head;
    qp->head = 0;
}

void push(Queue *qp, int number) {
    if (qp->tail == qp->size)
        input_queue(qp);
    qp->data[qp->tail++] = number;
}

int pop(Queue *qp) {
    if (qp->head != qp->tail)
        return qp->data[qp->head++];
    return 0;
}

void free_queue(Queue *qp) {
    free(qp->data);
    free(qp);
}

int head(Queue *qp){
    if (qp->head != qp->tail)
        return qp->data[qp->head];
    return 0;
}

void print_queue(Queue qp) {
    for (int i = qp.head; i < qp.tail; ++i)
        printf("%d ", qp.data[i]);
    printf("\n");
}

void test_queue() {
  Queue* q = create_queue(5);
  for (int i = 1; i < 3; ++i) {
    push(q, i * 2);
    printf("push>%d\n", i*2);
  }
  print_queue(*q);
  for (int i = 1; i < 25; ++i) {
    printf("pop>%d\n", pop(q));
  }
  for (int i = 1; i < 15; ++i) {
    push(q, i * 3);
    printf("push>%d\n", i*3);
  }
  for (int i = 1; i < 10; ++i) {
    printf("pop>%d\n", pop(q));
  }
  print_queue(*q);
  for (int i = 1; i < 4; ++i) {
    push(q, i * 5);
    printf("push>%d\n", i*5);
  }
  for (int i = 1; i < 2; ++i) {
    printf("pop>%d\n", pop(q));
  }
  print_queue(*q);
  for (int i = 1; i < 2; ++i) {
    push(q, i * 7);
    printf("push>%d\n", i*7);
  }
  print_queue(*q);
  free_queue(q);
}



int main(int argc, char *argv[]) {
/*
  Queue *queue_p;
  queue_p = create_queue(5);

  printf("Pop: %d\n", pop(queue_p));
  printf("Push: 10\n");
  push(queue_p, 10);
  printf("Push: 20\n");
  push(queue_p, 20);
  printf("Push: 30\n");
  push(queue_p, 30);
  printf("Push: 40\n");
  push(queue_p, 40);

  printf("Head: %d\n", head(queue_p));

  print_queue(*queue_p);

  printf("Pop: %d\n", pop(queue_p));
  printf("Pop: %d\n", pop(queue_p));
  printf("Pop: %d\n", pop(queue_p));
  printf("Pop: %d\n", pop(queue_p));
  printf("Pop: %d\n", pop(queue_p));

  free_queue(queue_p);*/
  test_queue();
  return 0;
}
