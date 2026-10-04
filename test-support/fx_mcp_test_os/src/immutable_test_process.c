#define _POSIX_C_SOURCE 200809L
#define _DARWIN_C_SOURCE
#include <errno.h>
#include <signal.h>
#include <poll.h>
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

static char **make_argv(const char *bytes, const int *offsets, int count)
{
    if (count < 1 || !bytes || !offsets) return NULL;
    char **argv = calloc((size_t)count + 1, sizeof(*argv));
    if (!argv) return NULL;
    for (int i = 0; i < count; ++i) argv[i] = (char *)bytes + offsets[i] - 1;
    return argv;
}

int fx_test_process_spawn(const char *bytes, const int *offsets, int count,
                          int *child_pid)
{
    char **argv = make_argv(bytes, offsets, count);
    if (!argv) return count < 1 ? EINVAL : ENOMEM;
    pid_t pid = fork();
    if (pid < 0) { int error = errno; free(argv); return error; }
    if (pid == 0) { execv(argv[0], argv); _exit(127); }
    *child_pid = (int)pid;
    free(argv);
    return 0;
}

int fx_test_process_spawn_piped(const char *bytes, const int *offsets, int count,
                                int capture_stderr, int *child_pid,
                                int *input_fd, int *output_fd)
{
    char **argv = make_argv(bytes, offsets, count);
    if (!argv) return count < 1 ? EINVAL : ENOMEM;
    int to_child[2], from_child[2];
    if (pipe(to_child)) { int error = errno; free(argv); return error; }
    if (pipe(from_child)) {
        int error = errno; close(to_child[0]); close(to_child[1]);
        free(argv); return error;
    }
    pid_t pid = fork();
    if (pid < 0) {
        int error = errno; close(to_child[0]); close(to_child[1]);
        close(from_child[0]); close(from_child[1]); free(argv); return error;
    }
    if (pid == 0) {
        close(to_child[1]); close(from_child[0]);
        if (dup2(to_child[0], STDIN_FILENO) < 0 ||
            dup2(from_child[1], STDOUT_FILENO) < 0 ||
            (capture_stderr && dup2(from_child[1], STDERR_FILENO) < 0)) _exit(126);
        close(to_child[0]); close(from_child[1]);
        execv(argv[0], argv); _exit(127);
    }
    close(to_child[0]); close(from_child[1]); free(argv);
    *child_pid = (int)pid; *input_fd = to_child[1]; *output_fd = from_child[0];
    return 0;
}

int fx_test_process_pipe_read(int fd, char *bytes, int capacity, int timeout_ms)
{
    if (fd < 0 || !bytes || capacity < 1) return -1;
    struct pollfd event = {fd, POLLIN | POLLHUP, 0};
    int ready;
    do { ready = poll(&event, 1, timeout_ms); } while (ready < 0 && errno == EINTR);
    if (ready == 0) return -2;
    if (ready < 0) return -1;
    ssize_t n;
    do { n = read(fd, bytes, (size_t)capacity); } while (n < 0 && errno == EINTR);
    return n < 0 ? -1 : (int)n;
}

int fx_test_process_pipe_write(int fd, const char *bytes, int count)
{
    if (fd < 0 || (!bytes && count > 0) || count < 0) return -1;
    sigset_t pipe_signal, old_mask, pending;
    sigemptyset(&pipe_signal); sigaddset(&pipe_signal, SIGPIPE);
    if (sigprocmask(SIG_BLOCK, &pipe_signal, &old_mask) != 0) return -1;
    sigpending(&pending);
    int had_pending = sigismember(&pending, SIGPIPE);
    int done = 0, error = 0;
    while (done < count) {
        ssize_t n = write(fd, bytes + done, (size_t)(count - done));
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) { error = n < 0 ? errno : EIO; break; }
        done += (int)n;
    }
    if (!had_pending && error == EPIPE) {
        sigset_t pending_after;
        if (sigpending(&pending_after) == 0 &&
            sigismember(&pending_after, SIGPIPE) == 1) {
            int received_signal;
            (void)sigwait(&pipe_signal, &received_signal);
        }
    }
    (void)sigprocmask(SIG_SETMASK, &old_mask, NULL);
    if (error) { errno = error; return -1; }
    return done;
}

int fx_test_process_close_fd(int fd) { return fd < 0 ? 0 : close(fd); }
int fx_test_process_is_executable(const char *path)
{ return path && access(path, X_OK) == 0; }

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
