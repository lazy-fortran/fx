#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

typedef struct {
    pid_t pid;
    int input;
    int output;
} fx_test_child;

static int fx_test_build(void) {
    pid_t pid = fork();
    int status = 0, elapsed = 0;
    if (pid < 0) return 0;
    if (pid == 0) {
        execlp("fpm", "fpm", "build", (char *)NULL);
        _exit(127);
    }
    while (elapsed <= 300000) {
        pid_t result = waitpid(pid, &status, WNOHANG);
        if (result == pid) return WIFEXITED(status) && WEXITSTATUS(status) == 0;
        if (result < 0 && errno != EINTR) return 0;
        struct timespec delay = {0, 10000000};
        nanosleep(&delay, NULL);
        elapsed += 10;
    }
    kill(pid, SIGKILL);
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
    return 0;
}

int fx_test_find_server(const char *test_binary, char *out, int capacity) {
    const char *override = getenv("FX_MCP_SERVER");
    const char *slash = strrchr(test_binary, '/');
    if (override && access(override, X_OK) == 0) {
        if (strlen(override) >= (size_t)capacity) return 0;
        strcpy(out, override);
        return 1;
    }
    if (!slash) return 0;
    size_t directory_len = (size_t)(slash - test_binary);
    if (directory_len + sizeof("/../app/fx-mcp-server") > (size_t)capacity) return 0;
    memcpy(out, test_binary, directory_len);
    strcpy(out + directory_len, "/../app/fx-mcp-server");
    if (!fx_test_build()) return 0;
    return access(out, X_OK) == 0;
}

void *fx_test_spawn(const char *path) {
    int in_pipe[2], out_pipe[2];
    pid_t pid;
    fx_test_child *child;
    if (pipe(in_pipe) || pipe(out_pipe)) return NULL;
    pid = fork();
    if (pid < 0) return NULL;
    if (pid == 0) {
        dup2(in_pipe[0], STDIN_FILENO);
        dup2(out_pipe[1], STDOUT_FILENO);
        close(in_pipe[0]); close(in_pipe[1]);
        close(out_pipe[0]); close(out_pipe[1]);
        execl(path, path, (char *)NULL);
        _exit(127);
    }
    close(in_pipe[0]); close(out_pipe[1]);
    child = malloc(sizeof(*child));
    if (!child) { kill(pid, SIGKILL); waitpid(pid, NULL, 0); return NULL; }
    child->pid = pid; child->input = in_pipe[1]; child->output = out_pipe[0];
    return child;
}

int fx_test_write(void *opaque, const char *bytes, int count) {
    fx_test_child *child = opaque;
    int done = 0;
    while (done < count) {
        ssize_t n = write(child->input, bytes + done, (size_t)(count - done));
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return -1;
        done += (int)n;
    }
    return done;
}

int fx_test_read(void *opaque, char *bytes, int capacity, int timeout_ms) {
    fx_test_child *child = opaque;
    struct pollfd pfd = { child->output, POLLIN, 0 };
    int ready;
    do { ready = poll(&pfd, 1, timeout_ms); } while (ready < 0 && errno == EINTR);
    if (ready <= 0) return ready == 0 ? 0 : -1;
    ssize_t n;
    do { n = read(child->output, bytes, (size_t)capacity); } while (n < 0 && errno == EINTR);
    return (int)n;
}

int fx_test_close(void *opaque, int timeout_ms) {
    fx_test_child *child = opaque;
    int status = 0, elapsed = 0;
    close(child->input); child->input = -1;
    while (elapsed <= timeout_ms) {
        pid_t result = waitpid(child->pid, &status, WNOHANG);
        if (result == child->pid) {
            close(child->output); free(child);
            return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
        }
        if (result < 0 && errno != EINTR) break;
        struct timespec delay = {0, 10000000};
        nanosleep(&delay, NULL); elapsed += 10;
    }
    kill(child->pid, SIGKILL);
    while (waitpid(child->pid, &status, 0) < 0 && errno == EINTR) {}
    close(child->output); free(child);
    return -2;
}
