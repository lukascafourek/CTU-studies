#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <pthread.h>

#define _PLACE_COUNT 7
#define _PRODUCT_COUNT 3

enum place {
    NUZKY, VRTACKA, OHYBACKA, SVARECKA, LAKOVNA, SROUBOVAK, FREZA
};

const char *place_str[_PLACE_COUNT] = {
    [NUZKY] = "nuzky",
    [VRTACKA] = "vrtacka",
    [OHYBACKA] = "ohybacka",
    [SVARECKA] = "svarecka",
    [LAKOVNA] = "lakovna",
    [SROUBOVAK] = "sroubovak",
    [FREZA] = "freza",
};

enum product {
    A, B, C
};

const char *product_str[_PRODUCT_COUNT] = {
    [A] = "A",
    [B] = "B",
    [C] = "C",
};

int find_string_in_array(const char **array, int length, char *what) {
    for (int i = 0; i < length; i++)
        if (strcmp(array[i], what) == 0)
            return i;
    return -1;
}

int ready_places[_PLACE_COUNT];
int empty_places[_PLACE_COUNT];

#define _PHASE_COUNT 6
int parts[_PRODUCT_COUNT][_PHASE_COUNT];

typedef struct worker_t {
    char *name;
    int workplace;
    pthread_t thread;
    bool active;
    bool end;
    int mutex;
    struct worker_t *next;
} worker_t;

bool got_eof = false, removed_place = false, var = false, working = false, working2 = false, working3 = false;
pthread_mutex_t main_mutex, people_mutex[14];
pthread_cond_t cond;
worker_t *workers = NULL;
worker_t *echo = NULL;
int a = 0, b = 0, c = 0, previous_a = -1, previous_b = -1, previous_c = -1, k = 0, more_pp[7] = {0};

bool worker_exit_eof(worker_t *worker) {
    if (!worker->active)
        return true;
    switch (worker->workplace) {
    case 0:
        if (parts[0][0] > 0 || parts[1][1] > 0)
            return false;
        if (a == 0 && b == 0)
            return true;
        if ((b == 0 || (b > 0 &&
            ((parts[1][2] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[1][0] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[1][5] > 0 && (ready_places[5] == 0 || ready_places[5] == empty_places[5])) || (parts[1][3] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[1][4] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4]))))) && 
            (a == 0 || (a > 0 && 
            ((parts[0][2] > 0 && (ready_places[2] == 0 || ready_places[2] == empty_places[2])) || (parts[0][1] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[0][3] > 0 && (ready_places[3] == 0 || ready_places[3] == empty_places[3])) || (parts[0][4] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[0][5] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4]))))))
            return true;
        break;
    case 1:
        if (removed_place)
            return true;
        if (parts[0][1] > 0 || parts[0][4] > 0 || parts[1][0] > 0 || parts[1][3] > 0 || parts[2][1] > 0 || parts[2][3] > 0)
            return false;
        if (a == 0 && b == 0 && c == 0)
            return true;
        if ((c == 0 || (c > 0 && 
            ((parts[2][0] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[2][2] > 0 && (ready_places[5] == 0 || ready_places[5] == empty_places[5])) ||
            (parts[2][4] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[2][5] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4]))))) 
            && (b == 0 || (b > 0 && 
            ((parts[1][2] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[1][5] > 0 && (ready_places[5] == 0 || ready_places[5] == empty_places[5])) ||
            (parts[1][1] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[1][4] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4]))))) 
            && (a == 0 || (a > 0 && 
            ((parts[0][0] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[0][2] > 0 && (ready_places[2] == 0 || ready_places[2] == empty_places[2])) ||
            (parts[0][3] > 0 && (ready_places[3] == 0 || ready_places[3] == empty_places[3])) || (parts[0][5] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4]))))))
            return true;
        break;
    case 2:
        if (parts[0][2] > 0)
            return false;
        if (a == 0)
            return true;
        if ((parts[0][0] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[0][1] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[0][3] > 0 && (ready_places[3] == 0 || ready_places[3] == empty_places[3])) || (parts[0][4] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[0][5] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4])))
            return true;
        break;
    case 3:
        if (parts[0][3] > 0)
            return false;
        if (a == 0)
            return true;
        if ((parts[0][0] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[0][1] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[0][2] > 0 && (ready_places[2] == 0 || ready_places[2] == empty_places[2])) || (parts[0][4] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) ||
            (parts[0][5] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4])))
            return true;
        break;
    case 4:
        if (parts[0][5] > 0 || parts[1][4] > 0 || parts[2][5] > 0)
            return false;
        if (a == 0 && b == 0 && c == 0)
            return true;
        if ((c == 0 || (c > 0 && 
            ((parts[2][0] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[2][2] > 0 && (ready_places[5] == 0 || ready_places[5] == empty_places[5])) ||
            (parts[2][4] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[2][1] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) || 
            (parts[2][3] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1]))))) && (b == 0 || (b > 0 && 
            ((parts[1][2] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[1][5] > 0 && (ready_places[5] == 0 || ready_places[5] == empty_places[5])) ||
            (parts[1][1] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[1][0] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) || 
            (parts[1][3] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1]))))) && (a == 0 || (a > 0 && 
            ((parts[0][0] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[0][2] > 0 && (ready_places[2] == 0 || ready_places[2] == empty_places[2])) ||
            (parts[0][3] > 0 && (ready_places[3] == 0 || ready_places[3] == empty_places[3])) || (parts[0][1] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) || 
            (parts[0][4] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1]))))))
            return true;
        break;
    case 5:
        if (parts[1][5] > 0 || parts[2][2] > 0)
            return false;
        if (b == 0 && c == 0)
            return true;
        if ((c == 0 || (c > 0 && 
            ((parts[2][0] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[2][5] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4])) ||
            (parts[2][4] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[2][1] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) || 
            (parts[2][3] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1]))))) && (b == 0 || (b > 0 && 
            ((parts[1][2] > 0 && (ready_places[6] == 0 || ready_places[6] == empty_places[6])) || (parts[1][4] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4])) ||
            (parts[1][1] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[1][0] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) || 
            (parts[1][3] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1]))))))
            return true;
        break;
    case 6:
        if (parts[1][2] > 0 || parts[2][0] > 0 || parts[2][4] > 0)
            return false;
        if (b == 0 && c == 0)
            return true;
        if ((c == 0 || (c > 0 && 
            ((parts[2][2] > 0 && (ready_places[5] == 0 || ready_places[5] == empty_places[5])) || (parts[2][5] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4])) ||
            (parts[2][1] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) || (parts[2][3] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1]))))) 
            && (b == 0 || (b > 0 && 
            ((parts[1][5] > 0 && (ready_places[5] == 0 || ready_places[5] == empty_places[5])) || (parts[1][4] > 0 && (ready_places[4] == 0 || ready_places[4] == empty_places[4])) ||
            (parts[1][1] > 0 && (ready_places[0] == 0 || ready_places[0] == empty_places[0])) || (parts[1][0] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1])) || 
            (parts[1][3] > 0 && (ready_places[1] == 0 || ready_places[1] == empty_places[1]))))))
            return true;
        break;
    }
    return false;
}

void *workers_thread(void* arg) {
    worker_t *worker = (worker_t*)arg;
    while (1) {
        switch (worker->workplace) {
            case 0:
                pthread_mutex_lock(&people_mutex[worker->mutex]);
                while ((!worker->active && !worker->end) || (parts[0][0] == 0 && parts[1][1] == 0 && !worker->end))
                    pthread_cond_wait(&cond, &people_mutex[worker->mutex]);
                if (!worker->end) {
                    if (more_pp[worker->workplace] == 2)
                        working = true;
                    if (parts[1][1] > 0) {
                        printf("%s nuzky 2 B\n", worker->name);
                        usleep(100000);
                        parts[1][1]--;
                        parts[1][2]++;
                    } else if (parts[0][0] > 0) {
                        printf("%s nuzky 1 A\n", worker->name);
                        usleep(100000);
                        parts[0][0]--;
                        parts[0][1]++;
                    }
                }
                if (got_eof && !working)
                    worker->end = worker_exit_eof(worker);
                if (worker->end) {
                    empty_places[worker->workplace]++;
                    printf("%s goes home\n", worker->name);
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    return NULL;
                }
                pthread_cond_broadcast(&cond);
                pthread_mutex_unlock(&people_mutex[worker->mutex]);
                break;
            case 1:
                pthread_mutex_lock(&people_mutex[worker->mutex]);
                while ((!worker->active && !worker->end) || ((parts[0][1] == 0 || (parts[0][1] > 0 && working3)) && parts[0][4] == 0 && 
                    (parts[1][0] == 0 || (parts[1][0] > 0 && working2 && strcmp("Vasek", worker->name) == 0)) && 
                    parts[1][3] == 0 && parts[2][1] == 0 && parts[2][3] == 0 && !worker->end) || removed_place) {
                    pthread_cond_wait(&cond, &people_mutex[worker->mutex]);
                    if (removed_place)
                        break;
                    }
                if (!worker->end && !removed_place) {
                    if (parts[0][4] > 0) {
                        printf("%s vrtacka 5 A\n", worker->name);
                        usleep(200000);
                        parts[0][4]--;
                        parts[0][5]++;
                    } else if (parts[1][3] > 0) {
                        printf("%s vrtacka 4 B\n", worker->name);
                        usleep(200000);
                        parts[1][3]--;
                        parts[1][4]++;
                    } else if (parts[2][3] > 0) {
                        printf("%s vrtacka 4 C\n", worker->name);
                        usleep(200000);
                        parts[2][3]--;
                        parts[2][4]++;
                    } else if (parts[0][1] > 0) {
                        if (more_pp[worker->workplace] == 2)
                            working3 = true;
                        printf("%s vrtacka 2 A\n", worker->name);
                        usleep(200000);
                        parts[0][1]--;
                        parts[0][2]++;
                    } else if (parts[2][1] > 0) {
                        printf("%s vrtacka 2 C\n", worker->name);
                        usleep(200000);
                        parts[2][1]--;
                        parts[2][2]++;
                    } else if (parts[1][0] > 0) {
                        printf("%s vrtacka 1 B\n", worker->name);
                        usleep(200000);
                        parts[1][0]--;
                        parts[1][1]++;
                    }
                }
                if (!var && removed_place) {
                    var = true;
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    continue;
                }
                if (got_eof) {
                    if (removed_place)
                        ready_places[worker->workplace]--;
                    worker->end = worker_exit_eof(worker);
                }
                if (worker->end) {
                    if (!removed_place)
                        empty_places[worker->workplace]++;
                    printf("%s goes home\n", worker->name);
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    return NULL;
                }
                pthread_cond_broadcast(&cond);
                pthread_mutex_unlock(&people_mutex[worker->mutex]);
                break;
            case 2:
                pthread_mutex_lock(&people_mutex[worker->mutex]);
                while ((!worker->active && !worker->end) || (parts[0][2] == 0 && !worker->end))
                    pthread_cond_wait(&cond, &people_mutex[worker->mutex]);
                if (!worker->end) {
                    if (parts[0][2] > 0) {
                        printf("%s ohybacka 3 A\n", worker->name);
                        usleep(150000);
                        parts[0][2]--;
                        parts[0][3]++;
                    }
                }
                if (got_eof)
                    worker->end = worker_exit_eof(worker);
                if (worker->end) {
                    empty_places[worker->workplace]++;
                    printf("%s goes home\n", worker->name);
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    return NULL;
                }
                pthread_cond_broadcast(&cond);
                pthread_mutex_unlock(&people_mutex[worker->mutex]);
                break;
            case 3:
                pthread_mutex_lock(&people_mutex[worker->mutex]);
                while ((!worker->active && !worker->end) || (parts[0][3] == 0 && !worker->end))
                    pthread_cond_wait(&cond, &people_mutex[worker->mutex]);
                if (!worker->end) {
                    if (parts[0][3] > 0) {
                        printf("%s svarecka 4 A\n", worker->name);
                        usleep(300000);
                        parts[0][3]--;
                        parts[0][4]++;
                    }
                }
                if (got_eof)
                    worker->end = worker_exit_eof(worker);
                if (worker->end) {
                    empty_places[worker->workplace]++;
                    printf("%s goes home\n", worker->name);
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    return NULL;
                }
                pthread_cond_broadcast(&cond);
                pthread_mutex_unlock(&people_mutex[worker->mutex]);
                break;
            case 4:
                pthread_mutex_lock(&people_mutex[worker->mutex]);
                while ((!worker->active && !worker->end) || (parts[0][5] == 0 && parts[1][4] == 0 && parts[2][5] == 0 && !worker->end))
                    pthread_cond_wait(&cond, &people_mutex[worker->mutex]);
                if (!worker->end) {
                    if (parts[0][5] > 0) {
                        printf("%s lakovna 6 A\n", worker->name);
                        usleep(400000);
                        parts[0][5]--;
                        printf("done A\n");
                        previous_a = a;
                        a--;
                    } else if (parts[2][5] > 0) {
                        printf("%s lakovna 6 C\n", worker->name);
                        usleep(400000);
                        parts[2][5]--;
                        printf("done C\n");
                        previous_c = c;
                        c--;
                    } else if (parts[1][4] > 0) {
                        printf("%s lakovna 5 B\n", worker->name);
                        usleep(400000);
                        parts[1][4]--;
                        parts[1][5]++;
                    }
                }
                if (got_eof)
                    worker->end = worker_exit_eof(worker);
                if (worker->end) {
                    empty_places[worker->workplace]++;
                    printf("%s goes home\n", worker->name);
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    return NULL;
                } else if (previous_a > a) {
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        if (copyForEof->workplace == 2 || copyForEof->workplace == 3)
                            copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                }
                pthread_cond_broadcast(&cond);
                pthread_mutex_unlock(&people_mutex[worker->mutex]);
                break;
            case 5:
                pthread_mutex_lock(&people_mutex[worker->mutex]);
                while ((!worker->active && !worker->end) || (parts[1][5] == 0 && parts[2][2] == 0 && !worker->end))
                    pthread_cond_wait(&cond, &people_mutex[worker->mutex]);
                if (!worker->end) {
                    if (parts[1][5] > 0) {
                        printf("%s sroubovak 6 B\n", worker->name);
                        usleep(250000);
                        parts[1][5]--;
                        printf("done B\n");
                        previous_b = b;
                        b--;
                    } else if (parts[2][2] > 0) {
                        printf("%s sroubovak 3 C\n", worker->name);
                        usleep(250000);
                        parts[2][2]--;
                        parts[2][3]++;
                    }
                }
                if (got_eof)
                    worker->end = worker_exit_eof(worker);
                if (worker->end) {
                    empty_places[worker->workplace]++;
                    printf("%s goes home\n", worker->name);
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    return NULL;
                } else if (previous_b > b) {
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        if (copyForEof->workplace == 0)
                            copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                }
                pthread_cond_broadcast(&cond);
                pthread_mutex_unlock(&people_mutex[worker->mutex]);
                break;
            case 6:
                pthread_mutex_lock(&people_mutex[worker->mutex]);
                while ((!worker->active && !worker->end) || (parts[1][2] == 0 && parts[2][0] == 0 && parts[2][4] == 0 && !worker->end))
                    pthread_cond_wait(&cond, &people_mutex[worker->mutex]);
                if (!worker->end) {
                    if (parts[2][4] > 0) {
                        printf("%s freza 5 C\n", worker->name);
                        usleep(500000);
                        parts[2][4]--;
                        parts[2][5]++;
                    } else if (parts[1][2] > 0) {
                        printf("%s freza 3 B\n", worker->name);
                        usleep(500000);
                        parts[1][2]--;
                        parts[1][3]++;
                    } else if (parts[2][0] > 0) {
                        printf("%s freza 1 C\n", worker->name);
                        usleep(500000);
                        parts[2][0]--;
                        parts[2][1]++;
                    }
                }
                if (got_eof)
                    worker->end = worker_exit_eof(worker);
                if (worker->end) {
                    empty_places[worker->workplace]++;
                    printf("%s goes home\n", worker->name);
                    worker_t *copyForEof = echo;
                    while (copyForEof) {
                        copyForEof->end = true;
                        copyForEof = copyForEof->next;
                    }
                    pthread_cond_broadcast(&cond);
                    pthread_mutex_unlock(&people_mutex[worker->mutex]);
                    return NULL;
                }
                pthread_cond_broadcast(&cond);
                pthread_mutex_unlock(&people_mutex[worker->mutex]);
                break;
        }
    }
    return NULL;
}

int main(int argc, char **argv) {
    pthread_mutex_init(&main_mutex, NULL);
    for (int i = 0; i < 14; i++)
        pthread_mutex_init(&people_mutex[i], NULL);
    pthread_cond_init(&cond, NULL);
    pthread_mutex_lock(&main_mutex);
    char *line, *cmd, *arg1, *arg2, ch;
    while (1) {
        int s = scanf(" %m[^\n]", &line), s2;
        if (s == 1) {
            if (strncmp(line, "start", 5) == 0) {
                s2 = sscanf(line, "%m[a-zA-Z] %m[a-zA-Z] %m[a-zA-Z]%c", &cmd, &arg1, &arg2, &ch);
                if ((s2 == 3) && (0 == strcmp(cmd, "start"))) {
                    int place = find_string_in_array(place_str, _PLACE_COUNT, arg2);
                    if (place >= 0) {
                        worker_t *new_worker = (worker_t*)malloc(sizeof(worker_t));
                        new_worker->name = strdup(arg1);
                        new_worker->workplace = place;
                        if (empty_places[place] > 0) {
                            empty_places[place]--;
                            new_worker->active = true;
                        } else
                            new_worker->active = false;
                        new_worker->end = false;
                        new_worker->mutex = k;
                        k++;
                        more_pp[place]++;
                        if (more_pp[place] == 2)
                            working2 = true;
                        new_worker->next = NULL;
                        if (!workers) {
                            workers = new_worker;
                            echo = new_worker;
                        } else {
                            worker_t *current = workers;
                            while (current->next)
                                current = current->next;
                            current->next = new_worker;
                        }
                        pthread_create(&new_worker->thread, NULL, workers_thread, new_worker);
                    }
                }
                if (s2 >= 3)
                    free(arg2);
            } else {
                s2 = sscanf(line, "%m[a-zA-Z] %m[a-zA-Z]%c", &cmd, &arg1, &ch);
                if ((s2 == 2) && (0 == strcmp(cmd, "make"))) {
                    int product = find_string_in_array(product_str, _PRODUCT_COUNT, arg1);
                    if (product >= 0) {
                        parts[product][0]++;
                        if (product == 0)
                            a++;
                        else if(product == 1)
                            b++;
                        else if (product == 2)
                            c++;
                        pthread_cond_broadcast(&cond);
                    }
                } else if ((s2 == 2) && (0 == strcmp(cmd, "end"))) {
                    worker_t *copyWorkers = workers; 
                    while (copyWorkers) {
                        if (strcmp(copyWorkers->name, arg1) == 0) {
                            copyWorkers->end = true;
                            pthread_cond_broadcast(&cond);
                            break;
                        }
                        copyWorkers = copyWorkers->next;
                    }
                } else if ((s2 == 2) && (0 == strcmp(cmd, "add"))) {
                    int place = find_string_in_array(place_str, _PLACE_COUNT, arg1);
                    if (place >= 0) {
                        ready_places[place]++;
                        empty_places[place]++;
                        worker_t *copyWorkers = workers;
                        while (copyWorkers) {
                            if (copyWorkers->workplace == place) {
                                copyWorkers->active = true;
                                empty_places[place]--;
                                pthread_cond_broadcast(&cond);
                                break;
                            }
                            copyWorkers = copyWorkers->next;
                        }
                    }
                } else if ((s2 == 2) && (0 == strcmp(cmd, "remove"))) {
                    int place = find_string_in_array(place_str, _PLACE_COUNT, arg1);
                    if (place >= 0) {
                        if (empty_places[place] > 0) {
                            ready_places[place]--;
                            empty_places[place]--;
                        } else {
                            worker_t *copyWorkers = workers;
                            while (copyWorkers) {
                                if (copyWorkers->workplace == place)
                                    removed_place = true;
                                copyWorkers = copyWorkers->next;
                            }
                        }
                    }
                }
            }
            if (s2 > 0)
                free(cmd);
            if (s2 > 1)
                free(arg1);
            free(line);
        } else if (s < 0)
            break;
    }
    worker_t *copyWorkers = workers;
    worker_t *copyForEof = workers;
    got_eof = true;
    while (copyForEof) {
        copyForEof->end = true;
        copyForEof = copyForEof->next;
    }
    pthread_cond_broadcast(&cond);
    pthread_mutex_unlock(&main_mutex);
    while (copyWorkers) {
        pthread_join(copyWorkers->thread, NULL);
        copyWorkers = copyWorkers->next;
    }
    while (workers) {
        worker_t *temp = workers;
        workers = workers->next;
        free(temp->name);
        free(temp);
    }
    pthread_cond_destroy(&cond);
    for (int i = 0; i < 14; i++)
        pthread_mutex_destroy(&people_mutex[i]);
    pthread_mutex_destroy(&main_mutex);
    return EXIT_SUCCESS;
}
