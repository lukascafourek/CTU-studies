#include "queue.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

bool increase_last_item_position(queue_t *queue);
void increase_first_item_position(queue_t *queue);

/**
 * @brief creates a new queue with a given size
 * 
 * @param capacity 
 * @return Returns NULL if error
 */
queue_t* create_queue(int capacity, bool allow_null){
   queue_t* new_queue = (queue_t*)malloc(sizeof(queue_t));
   //fprintf(stderr, "NEW queue\n");
   if (!new_queue){
      return new_queue;
   }
   new_queue->capacity = capacity;
   new_queue->array_beginning = (data_t*)malloc(sizeof(data_t)*capacity);
   new_queue->array_end = new_queue->array_beginning + capacity;
   new_queue->first_item_position = 0;
   new_queue->last_item_position = -1;
   new_queue->allow_null = allow_null;
   //fprintf(stderr, "CP: create_queue: capac: %i, ab: %p, fip: %i, lip: %i\n", new_queue->capacity, new_queue->array_beginning, new_queue->first_item_position, new_queue->last_item_position);
   return new_queue;
}

/* deletes the queue and all allocated memory */
void delete_queue(queue_t *queue){
   free(queue->array_beginning);
   free(queue);
}

/* 
 * inserts a reference to the element into the queue
 * returns: true on success; false otherwise
 */
bool push_to_queue(queue_t *queue, void *data){
   if (!queue || ((! queue->allow_null) && (! data))){
      fprintf(stderr, "Queue error: unsuccessfull push_to_queue");
      return false;
   }
   //fprintf(stderr, "CP: push: Error check done\n");
   bool return_value = increase_last_item_position(queue);
   //fprintf(stderr, "CP: push: Last item increased\n");
   if (!return_value){
      return false;
   }
   //fprintf(stderr, "CP: push: Last item increased succesfully\n");
   queue->array_beginning[queue->last_item_position] = data;
   //fprintf(stderr, "CP: push: Data saved on pos %i\n", queue->last_item_position);
   //fprintf(stderr, "FIP not changed: %i\n", queue->first_item_position);
   return true;
}

/* 
 * gets the first element from the queue and removes it from the queue
 * returns: the first element on success; NULL otherwise
 */
void* pop_from_queue(queue_t *queue){
    if (queue->last_item_position == -1){
        return NULL;
    }
    //fprintf(stderr, "CP: pop start: capac: %i, ab: %p, fip: %i, lip: %i\n", queue->capacity, queue->array_beginning, queue->first_item_position, queue->last_item_position);
    data_t return_value = queue->array_beginning[queue->first_item_position];
    //fprintf(stderr, "CP: return_value: %p, %i\n", return_value, *return_value);
    if (queue->last_item_position == queue->first_item_position){
        queue->last_item_position = -1;
        queue->first_item_position = 0;
        //fprintf(stderr, "FREE\n");
    }
    else{
        increase_first_item_position(queue);
    }
    //fprintf(stderr, "CP: pop end: capac: %i, ab: %p, fip: %i, lip: %i\n", queue->capacity, queue->array_beginning, queue->first_item_position, queue->last_item_position);
    return return_value;
}

/* 
 * gets idx-th element from the queue
 * returns the element that will be popped after idx calls of the pop_from_queue() 
 * returns: the idx-th element on success; NULL otherwise
 */
void* get_from_queue(queue_t *queue, uint64_t idx){
   if (get_queue_size(queue) <= idx || idx < 0){
      //fprintf(stderr, "NULL ret, FIP %i, LIP %i, idx %i, size: %i\n", queue->first_item_position, queue->last_item_position, idx, get_queue_size(queue));   
      return NULL;
   }
   else{
      //fprintf(stderr, "no NULL ret");
      return queue->array_beginning[(queue->first_item_position + idx) % queue->capacity];
   }
}

bool set_to_queue(queue_t *queue, uint64_t idx, void *data){
   if (get_queue_size(queue) <= idx || idx < 0){
      //fprintf(stderr, "NULL ret, FIP %i, LIP %i, idx %i, size: %i\n", queue->first_item_position, queue->last_item_position, idx, get_queue_size(queue));   
      return false;
   }
   else{
      //fprintf(stderr, "no NULL ret");
      queue->array_beginning[(queue->first_item_position + idx) % queue->capacity] = data;
      return true;
   }
}

/* gets number of stored elements */
uint64_t get_queue_size(queue_t *queue){
   //fprintf(stderr, "FIP: %i, LIP: %i ", queue->first_item_position, queue->last_item_position);
   if (queue->last_item_position == -1){
      //fprintf(stderr, "QSize: %i\n", 0);
      return 0;
   }
   int64_t size = (queue->last_item_position - queue->first_item_position) + 1;
   if (size <= 0){
      //fprintf(stderr, "QSize: %i\n", queue->capacity + size);
      return (uint64_t) (queue->capacity + size);
   }
   else{
      //fprintf(stderr, "QSize: %i\n", size);
      return (uint64_t) size;
   }
}


bool increase_last_item_position(queue_t *queue){
   if (queue->last_item_position == -1){
      queue->last_item_position = queue->first_item_position;
      return true;
   }
   int new_last_item_position = queue->last_item_position + 1;
   if (new_last_item_position >= (int)queue->capacity){
      new_last_item_position = 0;
   }
   if (new_last_item_position == queue->first_item_position){
      data_t* new_array = (data_t*)realloc(queue->array_beginning, sizeof(data_t)*(queue->capacity)*2);
      //fprintf(stderr, "Size increased: %i\n", queue->capacity*2);
      if (!new_array){
         return false;
      }
      queue->array_beginning = new_array;
      queue->capacity *= 2;
      if (queue->first_item_position != 0){
         memcpy(queue->array_beginning + queue->first_item_position + (queue->capacity / 2), queue->array_beginning + queue->first_item_position, ((queue->capacity / 2) - queue->first_item_position) * sizeof(data_t));
         queue->first_item_position = queue->first_item_position + (queue->capacity / 2);
         /*for (int i = queue->first_item_position; i != queue->last_item_position; (i++) % queue->capacity){
            fprintf(stderr, "%i", *((queue->array_beginning)[queue->first_item_position]));
         }*/
      }
      else{
         new_last_item_position = queue->capacity / 2;
      }
   }
   queue->last_item_position = new_last_item_position;
   return true;
}

void increase_first_item_position(queue_t *queue){
   queue->first_item_position++;
   if (queue->first_item_position >= (int)queue->capacity){
      //fprintf(stderr, "To zero\n");
      queue->first_item_position = 0;
   }
   if ((size_t)get_queue_size(queue) * 3 <= queue->capacity){
      if(queue->first_item_position == 0){

      }
      else if (queue->first_item_position <= queue->last_item_position){
         size_t size = get_queue_size(queue);
         memcpy(queue->array_beginning, queue->array_beginning + queue->first_item_position, size*sizeof(data_t));
         queue->first_item_position = 0;
         queue->last_item_position = size - 1;
      }
      else{
         memcpy(queue->array_beginning + queue->last_item_position + 1, queue->array_beginning + queue->first_item_position, (queue->capacity - queue->first_item_position)*sizeof(data_t));
         queue->first_item_position = queue->last_item_position + 1;
      }
      queue->capacity /= 3;
      queue->array_beginning = (data_t*)realloc(queue->array_beginning, sizeof(data_t)*queue->capacity);
      //fprintf(stderr, "Size shrinking\n");
   }
}
