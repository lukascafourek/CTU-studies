#include "queue.h"

queue_t* create_queue(int capacity) {
    queue_t* queue = (queue_t*)malloc(sizeof(queue_t));
        if (!queue) {
            return NULL;
        }
    queue->arr = (void*)malloc(sizeof(void*) * capacity);
    if (!queue->arr) {
        free(queue);
        return NULL;
    }
    queue->head = 0;
    queue->tail = 0;
    queue->count = 0;
    queue->capacity = capacity;
    return queue;
}
void delete_queue(queue_t *queue) {
    free(queue->arr);
    free(queue);
    queue = NULL;
}
bool push_to_queue(queue_t *queue, void *data) {
   if (queue->count < queue->capacity) {
      queue->arr[queue->tail] = data;
      queue->tail = (queue->tail + 1) % queue->capacity;
      queue->count += 1;
      return true;
   } else {
      return false;
   }
}
void* pop_from_queue(queue_t *queue) {
   void* ret = NULL;
   if (queue->count > 0) {
      ret = queue->arr[queue->head];
      queue->head = (queue->head + 1) % queue->capacity;
      queue->count -= 1;
   }
   return ret;
}
void* get_from_queue(queue_t *queue, int idx) {
    void* ret = NULL;
    if (idx < queue->count && idx >= 0) {
        if (queue->head + idx <= queue->capacity - 1) {
            ret = queue->arr[queue->head + idx];
        }
        else if (queue->head + idx > queue->capacity - 1) {
            ret = queue->arr[queue->head + idx - queue->capacity];
        }
    }
    return ret;
}
int get_queue_size(queue_t *queue) {
    return queue->count;
}
