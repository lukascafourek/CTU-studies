#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <time.h>

void signal_handler(int handler) {
    (void)handler;
    write(STDERR_FILENO, "GEN TERMINATED\n", 15);
    _exit(0);
}

void run_gen(int* pipefd) {
    close(pipefd[0]);
    if (dup2(pipefd[1], STDOUT_FILENO) == -1) {
        perror("ERROR: dup2 failed!");
        exit(1);
    }
    close(pipefd[1]);
    signal(SIGTERM, signal_handler);
    while (1) {
        printf("%d %d\n", rand() % 4096, rand() % 4096);
        fflush(stdout);
        sleep(1);
    }
}

void run_nsd(int* pipefd) {
    close(pipefd[1]);
    if (dup2(pipefd[0], STDIN_FILENO) == -1) {
        perror("ERROR: dup2 failed!");
        exit(1);
    }
    close(pipefd[0]);
    execl("nsd", "nsd", (char *)NULL);
    perror("ERROR: execl failed!");
    exit(2);
}

int main(int argc, char** argv) {
    srand(time(NULL));
    int pipefd[2];
    pid_t genpid, nsdpid;
    if (pipe(pipefd) == -1) {
        perror("ERROR: pipe failed!");
        exit(2);
    }
    if ((genpid = fork()) == -1) {
        perror("ERROR: fork for GEN failed!");
        exit(2);
    }
    if (genpid == 0) {
        run_gen(pipefd);
        exit(0);
    }
    if ((nsdpid = fork()) == -1) {
        perror("ERROR: fork for NSD failed!");
        exit(2);
    }
    if (nsdpid == 0) {
        run_nsd(pipefd);
        exit(0);
    }
    close(pipefd[0]);
    close(pipefd[1]);
    sleep(5);
    kill(genpid, SIGTERM);
    int status1, status2;
    if (wait(&status1) == -1) {
        perror("wait");
        exit(2);
    }
    if (wait(&status2) == -1) {
        perror("wait");
        exit(2);
    }
    if (WIFEXITED(status1) && WIFEXITED(status2) && WEXITSTATUS(status1) == 0 && WEXITSTATUS(status2) == 0) {
        printf("OK\n");
        exit(0);
    } else {
        printf("ERROR\n");
        exit(1);
    }
}
