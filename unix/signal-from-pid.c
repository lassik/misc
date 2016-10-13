#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void handle_sigalrm(int sig_num, siginfo_t *info, void *user_ptr)
{
    (void)sig_num;
    (void)user_ptr;
    if (info->si_code & SI_USER)
        printf("SIGALRM from pid %d\n", info->si_pid);
    else
        printf("SIGALRM not from user process\n");
}

extern int main(void)
{
    struct sigaction sa;
    int r;

    printf("my pid is %d\n", getpid());

    memset(&sa, 0, sizeof(struct sigaction));
    sa.sa_sigaction = handle_sigalrm;
    sa.sa_flags = SA_SIGINFO;
    r = sigaction(SIGALRM, &sa, NULL);
    if (r == -1) {
        fprintf(stderr, "sigaction: %s\n", strerror(errno));
        exit(1);
    }
    for (;;)
        sleep(1);

    return (0);
}
