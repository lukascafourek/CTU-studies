#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>

typedef struct node_t {
    int x;
    char *word;
    struct node_t *next;
} node_t;

typedef struct queue_t {
    node_t *head;
    node_t *tail;
    pthread_mutex_t mutex;
    sem_t semaphore;
} queue_t;

queue_t queue;

pthread_mutex_t print_mutex;

void push_to_queue(queue_t *queue, int x, char *word) {
    node_t *node = (node_t*)malloc(sizeof(node_t));
    if (!node) {
        fprintf(stderr, "ERROR: failed to allocate memory!\n");
        exit(1);
    }
    node->x = x;
    node->word = word;
    node->next = NULL;
    if (pthread_mutex_lock(&queue->mutex) != 0) {
        fprintf(stderr, "ERROR: failed to lock mutex!\n");
        exit(1);
    }
    if (queue->tail) {
        queue->tail->next = node;
        queue->tail = node;
    } else {
        queue->head = node;
        queue->tail = node;
    }
    if (pthread_mutex_unlock(&queue->mutex) != 0) {
        fprintf(stderr, "ERROR: failed to unlock mutex!\n");
        exit(1);
    }
    if (sem_post(&queue->semaphore) != 0) {
        fprintf(stderr, "ERROR: failed to post semaphore!\n");
        exit(1);
    }
}

node_t *pop_from_queue(queue_t *queue) {
    if (sem_wait(&queue->semaphore) != 0) {
        fprintf(stderr, "ERROR: failed to wait on semaphore!\n");
        exit(1);
    }
    if (pthread_mutex_lock(&queue->mutex) != 0) {
        fprintf(stderr, "ERROR: failed to lock mutex!\n");
        exit(1);
    }
    node_t *node = queue->head;
    queue->head = queue->head->next;
    if (!queue->head) {
        queue->tail = NULL;
    }
    if (pthread_mutex_unlock(&queue->mutex) != 0) {
        fprintf(stderr, "ERROR: failed to unlock mutex!\n");
        exit(1);
    }
    if (node->x == 0 && node->word == NULL) {
        free(node);
        return NULL;
    }
    return node;
}

void *producer_function(void *arg) {
    int r, x;
    char *word;
    while ((r = scanf("%d %ms", &x, &word)) == 2) {
        if (!word) {
            fprintf(stderr, "ERROR: failed to allocate memory for word!\n");
            exit(1);
        }
        if (x < 0) {
            fprintf(stderr, "ERROR: wrong input number!\n");
            exit(1);
        }
        push_to_queue(&queue, x, word);
    }
    if (r != EOF) {
        fprintf(stderr, "ERROR: wrong input word!\n");
        *(int*)arg = EXIT_FAILURE;
    }
    return NULL;
}

void *consumer_function(void *arg) {
    int thread_num = *(int*)arg;
    node_t *node = pop_from_queue(&queue);
    while (node) {
        if (pthread_mutex_lock(&print_mutex) != 0) {
            fprintf(stderr, "ERROR: failed to lock mutex!\n");
            exit(1);
        }
        printf("Thread %d:", thread_num);
        for (int i = 0; i < node->x; i++) {
            printf(" %s", node->word);
        }
        printf("\n");
        if (pthread_mutex_unlock(&print_mutex) != 0) {
            fprintf(stderr, "ERROR: failed to unlock mutex!\n");
            exit(1);
        }
        free(node->word);
        free(node);
        node = pop_from_queue(&queue);
    }
    return NULL;
}

int main(int argc, char **argv) {
    int n = argc > 1 ? atoi(argv[1]) : 1;
    int num_of_cpus = sysconf(_SC_NPROCESSORS_ONLN), ret = EXIT_SUCCESS;
    if (n < 1 || n > num_of_cpus) {
        fprintf(stderr, "ERROR: value of first argument is greater than number of CPUs!\n");
        return EXIT_FAILURE;
    }
    if (pthread_mutex_init(&queue.mutex, NULL) != 0) {
        fprintf(stderr, "ERROR: failed to initialize mutex!\n");
        return EXIT_FAILURE;
    }
    if (pthread_mutex_init(&print_mutex, NULL) != 0) {
        fprintf(stderr, "ERROR: failed to initialize print mutex!\n");
        return EXIT_FAILURE;
    }
    if (sem_init(&queue.semaphore, 0, 0) != 0) {
        fprintf(stderr, "ERROR: failed to initialize semaphore!\n");
        return EXIT_FAILURE;
    }
    pthread_t producer, consumer[n];
    if (pthread_create(&producer, NULL, producer_function, &ret) != 0) {
        fprintf(stderr, "ERROR: failed to create producer thread!\n");
        return EXIT_FAILURE;
    }
    int thread_num[n];
    for (int i = 0; i < n; i++) {
        thread_num[i] = i + 1;
        if (pthread_create(&consumer[i], NULL, consumer_function, &thread_num[i]) != 0) {
            fprintf(stderr, "ERROR: failed to create consumer thread %d!\n", i + 1);
            return EXIT_FAILURE;
        }
    }
    pthread_join(producer, NULL);
    for (int i = 0; i < n; ++i) {
        push_to_queue(&queue, 0, NULL);
    }
    for (int i = 0; i < n; ++i) {
        pthread_join(consumer[i], NULL);
    }
    if (pthread_mutex_destroy(&print_mutex) != 0) {
        fprintf(stderr, "ERROR: failed to destroy print mutex!\n");
        return EXIT_FAILURE;
    }
    if (pthread_mutex_destroy(&queue.mutex) != 0) {
        fprintf(stderr, "ERROR: failed to destroy mutex!\n");
        return EXIT_FAILURE;
    }
    if (sem_destroy(&queue.semaphore) != 0) {
        fprintf(stderr, "ERROR: failed to destroy semaphore!\n");
        return EXIT_FAILURE;
    }
    return ret;
}
