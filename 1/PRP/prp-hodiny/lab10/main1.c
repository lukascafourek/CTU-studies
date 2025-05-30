#include <stdio.h>
#include <stdlib.h>

typedef struct tWagon {
    int number;
    int priority;
    struct tWagon* next;
} Wagon;
void create_train(Wagon** t) {
    *t = NULL;
}
void push_train(Wagon** t, int n, int prior) {
    Wagon* b = (Wagon*)malloc(sizeof(Wagon));
    b->number = n;
    b->priority = prior;
    b->next = NULL;
    if (*t == NULL)
        *t = b;
    else if (prior <= (*t)->priority) {
        b->next = *t;
        *t = b;
    }
    else {
        Wagon* tmp = *t;
        while (tmp->next != NULL && prior > tmp->next->priority)
            tmp = tmp->next;
        b->next = tmp->next;
        tmp->next = b;
    }
}
void print_train(Wagon* t) {
    Wagon* tmp = t;
    while (tmp != NULL) {
        printf("(%d %d) -> ", tmp->number, tmp->priority);
        tmp = tmp->next;
    }
    printf("\n");
}
int main() {
    Wagon* Pendolino;
    create_train(&Pendolino);
    push_train(&Pendolino, 2, 2);
    push_train(&Pendolino, 2, 1);
    push_train(&Pendolino, 2, 7);
    push_train(&Pendolino, 2, 10);
    push_train(&Pendolino, 2, 5);
    print_train(Pendolino);
}
