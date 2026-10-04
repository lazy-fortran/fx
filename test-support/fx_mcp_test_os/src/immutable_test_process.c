#define _POSIX_C_SOURCE 200809L
#define _DARWIN_C_SOURCE
#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __APPLE__
#include <libproc.h>
#include <sys/sysctl.h>
#include <sys/user.h>
#endif

int fx_test_process_spawn(const char *bytes, const int *offsets, int count,
                          int *child_pid)
{
    char **argv = calloc((size_t)count + 1, sizeof(*argv));
    if (!argv) return ENOMEM;
    for (int i = 0; i < count; ++i) argv[i] = (char *)bytes + offsets[i] - 1;
    pid_t pid = fork();
    if (pid < 0) { int error = errno; free(argv); return error; }
    if (pid == 0) {
        execv(argv[0], argv);
        _exit(127);
    }
    *child_pid = (int)pid;
    free(argv);
    return 0;
}

int fx_test_process_wait_once(int child_pid, int *exit_status)
{
    if (child_pid <= 0) return -1;
    int status = 0;
    pid_t result;
    do { result = waitpid((pid_t)child_pid, &status, WNOHANG); }
    while (result < 0 && errno == EINTR);
    if (result == 0) return 0;
    if (result < 0) return -1;
    if (WIFEXITED(status)) *exit_status = WEXITSTATUS(status);
    else if (WIFSIGNALED(status)) *exit_status = 128 + WTERMSIG(status);
    else *exit_status = 255;
    return 1;
}

int fx_test_process_signal(int child_pid, int signal_number)
{
    if (child_pid <= 0) return EINVAL;
    return kill((pid_t)child_pid, signal_number) == 0 ? 0 : errno;
}

int fx_test_process_identity(int child_pid, int64_t *start, int *parent,
                             char *path, int capacity)
{
#ifdef __APPLE__
    struct kinfo_proc process;
    size_t length = sizeof(process);
    int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, child_pid};
    if (sysctl(mib, 4, &process, &length, NULL, 0) != 0 || length == 0)
        return errno ? errno : ESRCH;
    int copied = proc_pidpath(child_pid, path, (uint32_t)capacity);
    if (copied <= 0) return errno ? errno : ESRCH;
    *parent = process.kp_eproc.e_ppid;
    *start = (int64_t)process.kp_proc.p_starttime.tv_sec * 1000000 +
             process.kp_proc.p_starttime.tv_usec;
    return 0;
#else
    char proc_path[64], stat_line[4096];
    snprintf(proc_path, sizeof(proc_path), "/proc/%d/stat", child_pid);
    FILE *stat_file = fopen(proc_path, "r");
    if (!stat_file) return errno;
    if (!fgets(stat_line, sizeof(stat_line), stat_file)) {
        int error = errno ? errno : EIO;
        fclose(stat_file);
        return error;
    }
    fclose(stat_file);
    char *field = strrchr(stat_line, ')');
    if (!field || !field[1]) return EINVAL;
    field += 2;
    char *parent_field = strchr(field, ' ');
    if (!parent_field) return EINVAL;
    while (*parent_field == ' ') ++parent_field;
    char *parent_end = NULL;
    long parent_pid = strtol(parent_field, &parent_end, 10);
    if (parent_end == parent_field) return EINVAL;
    *parent = (int)parent_pid;
    for (int number = 3; number < 22; ++number) {
        field = strchr(field, ' ');
        if (!field) return EINVAL;
        while (*field == ' ') ++field;
    }
    char *end = NULL;
    unsigned long long ticks = strtoull(field, &end, 10);
    if (end == field) return EINVAL;
    *start = (int64_t)ticks;
    snprintf(proc_path, sizeof(proc_path), "/proc/%d/exe", child_pid);
    ssize_t copied = readlink(proc_path, path, (size_t)capacity - 1);
    if (copied < 0) return errno;
    if (copied >= capacity) return ENAMETOOLONG;
    path[copied] = '\0';
    return 0;
#endif
}

int64_t fx_test_process_clock_ms(void)
{
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
    return (int64_t)now.tv_sec * 1000 + now.tv_nsec / 1000000;
}

void fx_test_process_sleep_ms(int milliseconds)
{
    struct timespec delay = {milliseconds / 1000,
                             (milliseconds % 1000) * 1000000L};
    while (nanosleep(&delay, &delay) != 0 && errno == EINTR) {}
}
