#define _POSIX_C_SOURCE 200809L
#ifdef __linux__
#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

extern char **environ;
#define MAX_TRACEES 4096

static int64_t clock_ms(void)
{
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
    return (int64_t)now.tv_sec * 1000 + now.tv_nsec / 1000000;
}

/* Environment and argv storage are prepared in the parent. The forked child
 * uses only OS calls before execve: no allocator, environment mutation or
 * Fortran runtime is entered in the child. Overrides are caller policy. */
static char **environment(const char *bytes, const int *offsets, int count)
{
    int inherited = 0, used = 0;
    while (environ[inherited]) ++inherited;
    char **env = calloc((size_t)inherited + (size_t)count + 1, sizeof(*env));
    if (!env) return NULL;
    for (int i = 0; i < inherited; ++i) {
        int replaced = 0;
        for (int j = 0; j < count; ++j) {
            const char *override = bytes + offsets[j] - 1;
            const char *equal = strchr(override, '=');
            if (!equal) { free(env); errno = EINVAL; return NULL; }
            size_t key = (size_t)(equal - override) + 1;
            if (strncmp(environ[i], override, key) == 0) replaced = 1;
        }
        if (!replaced) env[used++] = environ[i];
    }
    for (int j = 0; j < count; ++j)
        env[used++] = (char *)bytes + offsets[j] - 1;
    return env;
}

static int append_identity(pid_t pid, char *paths, int capacity, int *used)
{
    char proc_path[64], executable[4096];
    snprintf(proc_path, sizeof(proc_path), "/proc/%ld/exe", (long)pid);
    ssize_t length = readlink(proc_path, executable, sizeof(executable) - 1);
    if (length < 0) return errno;
    if (*used + length + 2 > capacity) return ENOSPC;
    memcpy(paths + *used, executable, (size_t)length);
    *used += (int)length;
    paths[(*used)++] = '\n';
    paths[*used] = '\0';
    return 0;
}

static void kill_tracees(const pid_t *pids, int count)
{
    for (int i = 0; i < count; ++i) {
        if (pids[i] <= 0) continue;
        (void)kill(pids[i], SIGKILL);
        (void)ptrace(PTRACE_CONT, pids[i], NULL, NULL);
    }
}

int fx_test_process_trace_execs(const char *bytes, const int *offsets, int count,
                                const char *env_bytes, const int *env_offsets,
                                int env_count, char *exec_paths,
                                int paths_capacity, int *exit_status,
                                int timeout_ms)
{
    pid_t pids[MAX_TRACEES] = {0};
    int status, used = 0, result = 0, owned = 1, live = 1;
    const long options = PTRACE_O_TRACEFORK | PTRACE_O_TRACEVFORK |
                         PTRACE_O_TRACECLONE | PTRACE_O_TRACEEXEC |
                         PTRACE_O_EXITKILL;
    struct timespec pause = {0, 1000000L};
    if (!bytes || !offsets || count < 1 || !exec_paths ||
        paths_capacity < 2 || !exit_status || timeout_ms <= 0) return EINVAL;
    *exit_status = -1;
    exec_paths[0] = '\0';
    char **argv = calloc((size_t)count + 1, sizeof(*argv));
    if (!argv) return ENOMEM;
    for (int i = 0; i < count; ++i) argv[i] = (char *)bytes + offsets[i] - 1;
    char **env = environment(env_bytes, env_offsets, env_count);
    if (!env) { free(argv); return errno ? errno : ENOMEM; }
    int64_t started = clock_ms();
    if (started < 0) { free(argv); free(env); return errno; }
    pid_t root = fork();
    if (root == 0) {
        if (ptrace(PTRACE_TRACEME, 0, NULL, NULL) != 0) _exit(125);
        (void)kill(getpid(), SIGSTOP);
        execve(argv[0], argv, env);
        _exit(errno == ENOENT ? 127 : 126);
    }
    int fork_error = errno;
    free(argv);
    free(env);
    if (root < 0) return fork_error;
    pids[0] = root;
    while (live > 0) {
        int progressed = 0;
        if (!result && clock_ms() - started >= timeout_ms) result = ETIMEDOUT;
        if (result) kill_tracees(pids, owned);
        for (int i = 0; i < owned; ++i) {
            if (pids[i] <= 0) continue;
            pid_t pid = waitpid(pids[i], &status, __WALL | WNOHANG);
            if (pid == 0) continue;
            if (pid < 0) {
                if (errno == EINTR) continue;
                if (!result) result = errno;
                if (errno == ECHILD) { pids[i] = 0; --live; }
                continue;
            }
            progressed = 1;
            if (WIFEXITED(status) || WIFSIGNALED(status)) {
                if (pid == root)
                    *exit_status = WIFEXITED(status) ? WEXITSTATUS(status) :
                                   128 + WTERMSIG(status);
                pids[i] = 0;
                --live;
                continue;
            }
            if (!WIFSTOPPED(status)) continue;
            unsigned event = (unsigned)status >> 16;
            if (event == PTRACE_EVENT_FORK || event == PTRACE_EVENT_VFORK ||
                event == PTRACE_EVENT_CLONE) {
                unsigned long child = 0;
                if (ptrace(PTRACE_GETEVENTMSG, pid, NULL, &child) != 0) {
                    if (!result) result = errno;
                } else if (owned == MAX_TRACEES) {
                    (void)kill((pid_t)child, SIGKILL);
                    if (!result) result = ENOSPC;
                } else { pids[owned++] = (pid_t)child; ++live; }
            }
            if (event == PTRACE_EVENT_EXEC) {
                /* exec by a nonleader thread adopts the leader's PID. Retire
                 * its former TID so an expected ECHILD is not a trace failure. */
                unsigned long former = 0;
                if (ptrace(PTRACE_GETEVENTMSG, pid, NULL, &former) != 0) {
                    if (!result) result = errno;
                } else if ((pid_t)former != pid) {
                    for (int j = 0; j < owned; ++j) {
                        if (pids[j] == (pid_t)former) { pids[j] = 0; --live; }
                    }
                }
                int error = append_identity(pid, exec_paths, paths_capacity, &used);
                if (error && !result) result = error;
            }
            /* Apply options at every initial stop; they also propagate across
             * fork/clone. SIGSTOP is the tracing handshake, not a test signal. */
            if (ptrace(PTRACE_SETOPTIONS, pid, NULL, (void *)options) != 0 &&
                errno != ESRCH && !result) result = errno;
            int signal_number = WSTOPSIG(status);
            if (event || signal_number == SIGSTOP || signal_number == SIGTRAP)
                signal_number = 0;
            if (ptrace(PTRACE_CONT, pid, NULL,
                       (void *)(intptr_t)signal_number) != 0 &&
                errno != ESRCH && !result) result = errno;
        }
        if (!progressed) (void)nanosleep(&pause, NULL);
    }
    return result;
}
int fx_test_process_trace_supported(void) { return 1; }
#else
#include <errno.h>
int fx_test_process_trace_supported(void) { return 0; }
int fx_test_process_trace_execs(const char *bytes, const int *offsets, int count,
                                const char *env_bytes, const int *env_offsets,
                                int env_count, char *exec_paths,
                                int paths_capacity, int *exit_status,
                                int timeout_ms)
{
    (void)bytes; (void)offsets; (void)count; (void)env_bytes;
    (void)env_offsets; (void)env_count; (void)exec_paths;
    (void)paths_capacity; (void)exit_status; (void)timeout_ms;
    return ENOTSUP;
}
#endif
