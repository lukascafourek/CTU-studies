#ifndef __QUEUE_H__
#define __QUEUE_H__

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

typedef int* data_t;

/**
 * @brief Queue structure which holds all necessary data
 */
typedef struct {
    size_t capacity; // stores capacity of the queue
    data_t* array_beginning;
    data_t* array_end;
    int first_item_position;
    int last_item_position; // index of last valid item, -1 if not exist
    bool allow_null;
} queue_t;

/* creates a new queue with a given size */
queue_t* create_queue(int capacity, bool allow_null);

/* deletes the queue and all allocated memory */
void delete_queue(queue_t *queue);

/* 
 * inserts a reference to the element into the queue
 * returns: true on success; false otherwise
 */
bool push_to_queue(queue_t *queue, void *data);

/* 
 * gets the first element from the queue and removes it from the queue
 * returns: the first element on success; NULL otherwise
 */
void* pop_from_queue(queue_t *queue);

/* 
 * gets idx-th element from the queue
 * returns the element that will be popped after idx calls of the pop_from_queue() 
 * returns: the idx-th element on success; NULL otherwise
 */
void* get_from_queue(queue_t *queue, uint64_t idx);

bool set_to_queue(queue_t *queue, uint64_t idx, void *data); 

/* gets number of stored elements */
uint64_t get_queue_size(queue_t *queue);

#endif /* __QUEUE_H__ */
