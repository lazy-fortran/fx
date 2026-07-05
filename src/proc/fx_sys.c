#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#ifdef __linux__
#include <sys/inotify.h>
#elif defined(__APPLE__)
#include <sys/event.h>
#endif
#include <dirent.h>
#include <poll.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#if defined(__linux__) || defined(__APPLE__)
static int fx_inotify_force_enospc_once = 0;
#endif

/*
 * fx_sys.c: C implementations for fx_proc.f90 Fortran interfaces.
 * Provides process execution, directory scanning, file I/O, and
 * signal handling via POSIX APIs.
 */

/* Slot size for fx_c_scan_dir output: matches character(len=512) in fx_proc.f90 */
#define FX_SCAN_SLOT 512

struct fx_path_list {
    char **items;
    size_t n;
    size_t cap;
};

static int fx_path_list_add(struct fx_path_list *list, const char *path)
{
    char **next;
    size_t next_cap;

    if (list->n == list->cap) {
        next_cap = list->cap == 0 ? 64 : list->cap * 2;
        next = realloc(list->items, next_cap * sizeof(char *));
        if (next == NULL) return -1;
        list->items = next;
        list->cap = next_cap;
    }
    list->items[list->n] = strdup(path);
    if (list->items[list->n] == NULL) return -1;
    list->n++;
    return 0;
}

static void fx_path_list_free(struct fx_path_list *list)
{
    size_t i;
    for (i = 0; i < list->n; i++) free(list->items[i]);
    free(list->items);
    list->items = NULL;
    list->n = 0;
    list->cap = 0;
}

static int fx_path_cmp(const void *lhs, const void *rhs)
{
    const char *const *a = lhs;
    const char *const *b = rhs;
    return strcmp(*a, *b);
}

static int fx_scan_skip_dir(const char *name)
{
    return strcmp(name, ".") == 0 || strcmp(name, "..") == 0 ||
           strcmp(name, ".git") == 0 || strcmp(name, "build") == 0 ||
           strcmp(name, "node_modules") == 0;
}

static int fx_has_extension(const char *path, const char *extensions, int n_ext)
{
    const char *dot;
    const char *p;
    int i;

    dot = strrchr(path, '.');
    if (dot == NULL) return 0;
    p = extensions;
    for (i = 0; i < n_ext; i++) {
        if (strcmp(dot, p) == 0) return 1;
        p += strlen(p) + 1;
    }
    return 0;
}

/* !$omp parallel: future parallelism for large directory trees */
static int fx_scan_dir_rec(const char *root, const char *extensions, int n_ext,
                            struct fx_path_list *list)
{
    DIR *dir;
    struct dirent *entry;
    char path[PATH_MAX];
    struct stat st;

    dir = opendir(root);
    if (dir == NULL) return (errno == ENOENT) ? 0 : -1;

    while ((entry = readdir(dir)) != NULL) {
        if (fx_scan_skip_dir(entry->d_name)) continue;
        snprintf(path, sizeof(path), "%s/%s", root, entry->d_name);
        if (stat(path, &st) != 0) continue;
        if (S_ISDIR(st.st_mode)) {
            if (fx_scan_dir_rec(path, extensions, n_ext, list) != 0) {
                closedir(dir);
                return -1;
            }
        } else if (S_ISREG(st.st_mode)) {
            if (fx_has_extension(path, extensions, n_ext)) {
                if (fx_path_list_add(list, path) != 0) {
                    closedir(dir);
                    return -1;
                }
            }
        }
    }

    closedir(dir);
    return 0;
}

/* Fork/execvp with stdout+stderr capture via pipes. Uses poll() to avoid deadlock. */
int fx_c_exec(const char *argv, int n_argv,
              char *stdout_buf, int *stdout_len,
              char *stderr_buf, int *stderr_len)
{
    char **args;
    const char *p;
    int out_pipe[2], err_pipe[2];
    pid_t pid;
    int status;
    int max_out, max_err;
    int n_out, n_err;
    int done_out, done_err;
    struct pollfd pfds[2];
    char discard[4096];
    ssize_t got;
    int i;

    args = malloc(((size_t)n_argv + 1) * sizeof(char *));
    if (args == NULL) return -1;
    p = argv;
    for (i = 0; i < n_argv; i++) {
        args[i] = (char *)p;
        p += strlen(p) + 1;
    }
    args[n_argv] = NULL;

    max_out = *stdout_len;
    max_err = *stderr_len;
    *stdout_len = 0;
    *stderr_len = 0;

    if (pipe(out_pipe) < 0 || pipe(err_pipe) < 0) {
        free(args);
        return -1;
    }

    pid = fork();
    if (pid < 0) {
        close(out_pipe[0]); close(out_pipe[1]);
        close(err_pipe[0]); close(err_pipe[1]);
        free(args);
        return -1;
    }

    if (pid == 0) {
        close(out_pipe[0]);
        close(err_pipe[0]);
        if (dup2(out_pipe[1], STDOUT_FILENO) < 0) _exit(126);
        if (dup2(err_pipe[1], STDERR_FILENO) < 0) _exit(126);
        close(out_pipe[1]);
        close(err_pipe[1]);
        execvp(args[0], args);
        _exit(errno == ENOENT ? 127 : 126);
    }

    close(out_pipe[1]);
    close(err_pipe[1]);
    free(args);

    done_out = 0;
    done_err = 0;
    n_out = 0;
    n_err = 0;
    pfds[0].fd = out_pipe[0];
    pfds[0].events = POLLIN;
    pfds[1].fd = err_pipe[0];
    pfds[1].events = POLLIN;

    while (!done_out || !done_err) {
        pfds[0].fd = done_out ? -1 : out_pipe[0];
        pfds[1].fd = done_err ? -1 : err_pipe[0];
        pfds[0].revents = 0;
        pfds[1].revents = 0;

        if (poll(pfds, 2, -1) < 0) {
            if (errno == EINTR) continue;
            break;
        }

        if (!done_out && (pfds[0].revents & (POLLIN | POLLHUP | POLLERR))) {
            if (n_out < max_out) {
                got = read(out_pipe[0], stdout_buf + n_out,
                           (size_t)(max_out - n_out));
            } else {
                got = read(out_pipe[0], discard, sizeof(discard));
            }
            if (got <= 0) done_out = 1;
            else if (n_out < max_out) n_out += (int)got;
        }

        if (!done_err && (pfds[1].revents & (POLLIN | POLLHUP | POLLERR))) {
            if (n_err < max_err) {
                got = read(err_pipe[0], stderr_buf + n_err,
                           (size_t)(max_err - n_err));
            } else {
                got = read(err_pipe[0], discard, sizeof(discard));
            }
            if (got <= 0) done_err = 1;
            else if (n_err < max_err) n_err += (int)got;
        }
    }

    close(out_pipe[0]);
    close(err_pipe[0]);
    *stdout_len = n_out;
    *stderr_len = n_err;

    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 1;
}

/* Fork/execvp without output capture. Returns exit code. */
int fx_c_exec_silent(const char *argv, int n_argv)
{
    char **args;
    const char *p;
    pid_t pid;
    int status;
    int i;

    args = malloc(((size_t)n_argv + 1) * sizeof(char *));
    if (args == NULL) return -1;
    p = argv;
    for (i = 0; i < n_argv; i++) {
        args[i] = (char *)p;
        p += strlen(p) + 1;
    }
    args[n_argv] = NULL;

    pid = fork();
    if (pid < 0) { free(args); return -1; }

    if (pid == 0) {
        execvp(args[0], args);
        _exit(errno == ENOENT ? 127 : 126);
    }

    free(args);
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 1;
}

/*
 * Recursive directory scan with extension filtering.
 * extensions: flat null-terminated strings, n_ext entries.
 * files: output slots of FX_SCAN_SLOT bytes each (matches character(len=512)).
 * Skips .git, build, node_modules. Sorts results.
 */
int fx_c_scan_dir(const char *root, const char *extensions, int n_ext,
                  char *files, int *n_files, int max_files)
{
    struct fx_path_list list;
    size_t i;
    char *slot;

    *n_files = 0;
    list.items = NULL;
    list.n = 0;
    list.cap = 0;

    if (fx_scan_dir_rec(root, extensions, n_ext, &list) != 0) {
        fx_path_list_free(&list);
        return -1;
    }

    qsort(list.items, list.n, sizeof(char *), fx_path_cmp);

    for (i = 0; i < list.n && (int)i < max_files; i++) {
        slot = files + i * FX_SCAN_SLOT;
        memset(slot, 0, FX_SCAN_SLOT);
        strncpy(slot, list.items[i], FX_SCAN_SLOT - 1);
    }

    *n_files = (int)(list.n < (size_t)max_files ? list.n : (size_t)max_files);
    fx_path_list_free(&list);
    return 0;
}

/* Read entire file into buffer. n_bytes: in=max, out=actual. */
int fx_c_file_read(const char *path, char *content, int *n_bytes)
{
    int fd;
    ssize_t got;
    int max, total;

    max = *n_bytes;
    *n_bytes = 0;
    fd = open(path, O_RDONLY);
    if (fd < 0) return -1;

    total = 0;
    while (total < max) {
        got = read(fd, content + total, (size_t)(max - total));
        if (got < 0) {
            if (errno == EINTR) continue;
            close(fd);
            return -1;
        }
        if (got == 0) break;
        total += (int)got;
    }

    close(fd);
    *n_bytes = total;
    return 0;
}

/* Write buffer to file. Returns 0 on success, -1 on error. */
int fx_c_file_write(const char *path, const char *content, int n_bytes)
{
    int fd;
    ssize_t written;
    int total;

    fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (fd < 0) return -1;

    total = 0;
    while (total < n_bytes) {
        written = write(fd, content + total, (size_t)(n_bytes - total));
        if (written < 0) {
            if (errno == EINTR) continue;
            close(fd);
            return -1;
        }
        total += (int)written;
    }

    close(fd);
    return 0;
}

/* Create a temp file. path_len receives the length of the path written. */
void fx_c_tmpfile(const char *prefix, char *path, int *path_len)
{
    char tpl[PATH_MAX];
    int fd;
    int len;

    path[0] = '\0';
    *path_len = 0;

    if (prefix != NULL && prefix[0] != '\0') {
        len = snprintf(tpl, sizeof(tpl), "%sXXXXXX", prefix);
    } else {
        len = snprintf(tpl, sizeof(tpl), "/tmp/fx_XXXXXX");
    }

    if (len < 0 || len >= (int)sizeof(tpl)) return;

    fd = mkstemp(tpl);
    if (fd < 0) return;
    close(fd);

    len = (int)strlen(tpl);
    memcpy(path, tpl, (size_t)len + 1);
    *path_len = len;
}

/* Return current process ID. */
int fx_c_pid(void)
{
    return (int)getpid();
}

/* Send signal to process. Returns 0 on success, -1 on error. */
int fx_c_kill(int pid, int signal)
{
    return kill((pid_t)pid, signal);
}

#if defined(__linux__) || defined(__APPLE__)
void fx_c_inotify_test_force_enospc_once(void)
{
    fx_inotify_force_enospc_once = 1;
}
#endif

int fx_c_stderr_redirect(const char *path)
{
    int target_fd;
    int saved_fd;

    fflush(stderr);
    target_fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (target_fd < 0) return -1;

    saved_fd = dup(STDERR_FILENO);
    if (saved_fd < 0) {
        close(target_fd);
        return -1;
    }

    if (dup2(target_fd, STDERR_FILENO) < 0) {
        close(target_fd);
        close(saved_fd);
        return -1;
    }

    close(target_fd);
    return saved_fd;
}

int fx_c_stderr_restore(int saved_fd)
{
    int rc;

    if (saved_fd < 0) return 0;
    fflush(stderr);
    rc = dup2(saved_fd, STDERR_FILENO);
    close(saved_fd);
    fflush(stderr);
    return (rc < 0) ? -1 : 0;
}

/* Framing state used for MCP input/output.
 * -1 = unknown, 0 = bare JSON, 1 = Content-Length.
 */
static int fx_mcp_framing = -1;

void fx_c_read_jsonrpc_message(char *buf, int bufsize, int *nread) {
    int content_length = -1;
    int pos = 0;
    int is_json = 0;
    int saw_header = 0;
    int ch;
    char discard[256];
    int remaining;
    size_t n;
    size_t got;

    *nread = 0;
    if (bufsize <= 0) return;

    for (;;) {
        pos = 0;
        is_json = 0;
        for (;;) {
            ch = fgetc(stdin);
            if (ch == EOF) {
                if (pos == 0 && !saw_header) {
                    *nread = -1;
                } else {
                    *nread = -2;
                }
                return;
            }
            if (ch == '\r') continue;
            if (ch == '\n') break;

            if (pos == 0 && isspace((unsigned char)ch)) continue;
            if (pos == 0) is_json = (ch == '{');

            if (is_json) {
                if (pos < bufsize) buf[pos++] = (char) ch;
            } else {
                if (pos < bufsize - 1) buf[pos++] = (char) ch;
            }
        }

        if (pos == 0) {
            if (content_length > 0) break;
            if (saw_header) {
                *nread = -2;
                return;
            }
            continue;
        }

        if (is_json) {
            *nread = pos < bufsize ? pos : bufsize;
            if (fx_mcp_framing < 0) fx_mcp_framing = 0;
            return;
        }

        buf[pos] = '\0';
        if (strncasecmp(buf, "content-length:", 15) == 0) {
            char *p = buf + 15;
            if (fx_mcp_framing < 0) fx_mcp_framing = 1;
            saw_header = 1;
            while (isspace((unsigned char)*p)) p++;
            content_length = atoi(p);
            if (content_length <= 0) {
                *nread = -2;
                return;
            }
            /* do not break: continue reading headers until blank line */
            continue;
        }
        saw_header = 1;
        if (fx_mcp_framing < 0) fx_mcp_framing = 1;
    }

    if (content_length <= 0) {
        *nread = -2;
        return;
    }

    if (content_length > bufsize) {
        remaining = content_length;
        while (remaining > 0) {
            n = (size_t)(remaining < (int)sizeof(discard) ? remaining : (int)sizeof(discard));
            got = fread(discard, 1, n, stdin);
            if (got == 0) {
                *nread = -2;
                return;
            }
            remaining -= (int)got;
        }
        *nread = -2;
        return;
    }

    if (fx_mcp_framing < 0) fx_mcp_framing = 1;

    {
        int total = 0;
        while (total < content_length) {
            size_t got = fread(buf + total, 1, (size_t)(content_length - total), stdin);
            if (got == 0) {
                *nread = -2;
                return;
            }
            total += (int)got;
        }
        *nread = total;
    }
}

int fx_c_get_mcp_framing(void) {
    return fx_mcp_framing;
}

#ifdef __linux__
typedef struct watch_entry {
    int wd;
    char *path;
    struct watch_entry *next;
} watch_entry_t;

typedef struct watch_state {
    int fd;
    watch_entry_t *watches;
    unsigned char pending[sizeof(struct inotify_event) + PATH_MAX + 1];
    size_t pending_len;
    size_t pending_pos;
    struct watch_state *next;
} watch_state_t;

static watch_state_t *fx_watch_states = NULL;
#endif /* __linux__ */

static void fx_trim_path(const char *path, char *out, size_t out_len)
{
    size_t len;

    if (out_len == 0) return;
    if (path == NULL) {
        out[0] = '\0';
        return;
    }

    len = strlen(path);
    while (len > 1 && path[len - 1] == '/') len--;
    if (len >= out_len) len = out_len - 1;
    memcpy(out, path, len);
    out[len] = '\0';
}

static int fx_join_path(char *out, size_t out_len, const char *base,
                        const char *name)
{
    char clean_base[PATH_MAX];
    size_t len;

    fx_trim_path(base, clean_base, sizeof(clean_base));
    if (strcmp(clean_base, "/") == 0) {
        return snprintf(out, out_len, "/%s", name);
    }

    len = strlen(clean_base);
    if (len == 0) {
        return snprintf(out, out_len, "%s", name);
    }

    return snprintf(out, out_len, "%s/%s", clean_base, name);
}

static int fx_path_kind(const char *path, int *is_dir, int *is_symlink_dir)
{
    struct stat lst;
    struct stat st;

    if (is_dir) *is_dir = 0;
    if (is_symlink_dir) *is_symlink_dir = 0;

    if (lstat(path, &lst) != 0) return -1;
    if (S_ISLNK(lst.st_mode)) {
        if (is_symlink_dir) *is_symlink_dir = 1;
        if (stat(path, &st) != 0) return -1;
        if (is_dir) *is_dir = S_ISDIR(st.st_mode);
        return 0;
    }

    if (is_dir) *is_dir = S_ISDIR(lst.st_mode);
    return 0;
}

static int fx_mkdir_p(const char *path)
{
    char clean[PATH_MAX];
    char parent[PATH_MAX];
    char *slash;
    struct stat st;
    size_t parent_len;

    if (path == NULL) return -1;

    fx_trim_path(path, clean, sizeof(clean));
    if (clean[0] == '\0') return -1;
    if (strcmp(clean, "/") == 0) return 0;

    if (stat(clean, &st) == 0) {
        return S_ISDIR(st.st_mode) ? 0 : -1;
    }

    slash = strrchr(clean, '/');
    if (slash != NULL && slash != clean) {
        parent_len = (size_t) (slash - clean);
        if (parent_len >= sizeof(parent)) return -1;
        memcpy(parent, clean, parent_len);
        parent[parent_len] = '\0';
        if (fx_mkdir_p(parent) != 0) return -1;
    }

    if (mkdir(clean, 0777) != 0 && errno != EEXIST) return -1;
    return 0;
}

int fx_c_mkdir_p(const char *path)
{
    return fx_mkdir_p(path);
}

int fx_c_rename(const char *src, const char *dst)
{
    if (rename(src, dst) == 0) return 0;
    return -1;
}

int fx_c_unlink(const char *path)
{
    if (unlink(path) == 0) return 0;
    if (errno == ENOENT) return 0;
    return -1;
}

int fx_c_rmdir(const char *path)
{
    if (rmdir(path) == 0) return 0;
    if (errno == ENOENT) return 0;
    return -1;
}

int fx_c_file_stat(const char *path, long long *size_bytes, long long *mtime)
{
    struct stat st;

    if (size_bytes != NULL) *size_bytes = 0;
    if (mtime != NULL) *mtime = 0;

    if (stat(path, &st) != 0) return -1;
    if (size_bytes != NULL) *size_bytes = (long long) st.st_size;
    if (mtime != NULL) *mtime = (long long) st.st_mtime;
    return 0;
}

int fx_c_file_fingerprint(const char *path, long long *size_bytes,
                          long long *mtime_ns)
{
    struct stat st;

    if (size_bytes != NULL) *size_bytes = 0;
    if (mtime_ns != NULL) *mtime_ns = 0;

    if (stat(path, &st) != 0) return -1;
    if (size_bytes != NULL) *size_bytes = (long long) st.st_size;
#if defined(__APPLE__)
    if (mtime_ns != NULL) *mtime_ns = (long long) st.st_mtimespec.tv_sec *
                                          1000000000LL +
                                      (long long) st.st_mtimespec.tv_nsec;
#else
    if (mtime_ns != NULL) *mtime_ns = (long long) st.st_mtim.tv_sec *
                                          1000000000LL +
                                      (long long) st.st_mtim.tv_nsec;
#endif
    return 0;
}

long long fx_c_unix_time(void)
{
    return (long long) time(NULL);
}

/* Set a file's access and modification time to mtime (unix seconds).
   Portable across Linux and macOS via utimes(); BSD touch lacks GNU's
   `-d "N hours ago"`, so tests set mtime directly through this. */
int fx_c_set_mtime(const char *path, long long mtime)
{
    struct timeval times[2];

    times[0].tv_sec = (time_t) mtime;
    times[0].tv_usec = 0;
    times[1].tv_sec = (time_t) mtime;
    times[1].tv_usec = 0;
    if (utimes(path, times) != 0) return -1;
    return 0;
}

#ifdef __linux__
static watch_state_t *fx_watch_state_find(int fd)
{
    watch_state_t *state;

    for (state = fx_watch_states; state != NULL; state = state->next) {
        if (state->fd == fd) return state;
    }
    return NULL;
}

static watch_state_t *fx_watch_state_add(int fd)
{
    watch_state_t *state;

    state = (watch_state_t *)calloc(1, sizeof(*state));
    if (state == NULL) return NULL;
    state->fd = fd;
    state->next = fx_watch_states;
    fx_watch_states = state;
    return state;
}

static void fx_watch_entry_free(watch_entry_t *entry)
{
    if (entry == NULL) return;
    free(entry->path);
    free(entry);
}

static void fx_watch_state_free(watch_state_t *state)
{
    watch_entry_t *entry;
    watch_entry_t *next;

    if (state == NULL) return;
    entry = state->watches;
    while (entry != NULL) {
        next = entry->next;
        fx_watch_entry_free(entry);
        entry = next;
    }
    free(state);
}

static void fx_watch_state_remove_fd(int fd)
{
    watch_state_t *state;
    watch_state_t *prev;

    prev = NULL;
    state = fx_watch_states;
    while (state != NULL) {
        if (state->fd == fd) {
            if (prev == NULL) {
                fx_watch_states = state->next;
            } else {
                prev->next = state->next;
            }
            fx_watch_state_free(state);
            return;
        }
        prev = state;
        state = state->next;
    }
}

static watch_entry_t *fx_watch_state_find_wd(watch_state_t *state, int wd,
                                             watch_entry_t **prev_out)
{
    watch_entry_t *entry;
    watch_entry_t *prev;

    if (prev_out != NULL) *prev_out = NULL;
    if (state == NULL) return NULL;

    prev = NULL;
    entry = state->watches;
    while (entry != NULL) {
        if (entry->wd == wd) {
            if (prev_out != NULL) *prev_out = prev;
            return entry;
        }
        prev = entry;
        entry = entry->next;
    }
    return NULL;
}

static void fx_watch_state_store(watch_state_t *state, int wd,
                                 const char *path)
{
    watch_entry_t *entry;
    watch_entry_t *prev;
    char clean[PATH_MAX];
    size_t len;

    if (state == NULL) return;
    entry = fx_watch_state_find_wd(state, wd, &prev);
    fx_trim_path(path, clean, sizeof(clean));
    if (entry != NULL) {
        free(entry->path);
        entry->path = strdup(clean);
        return;
    }

    entry = (watch_entry_t *)calloc(1, sizeof(*entry));
    if (entry == NULL) return;
    len = strlen(clean);
    entry->path = (char *)malloc(len + 1);
    if (entry->path == NULL) {
        free(entry);
        return;
    }
    memcpy(entry->path, clean, len + 1);
    entry->wd = wd;
    entry->next = state->watches;
    state->watches = entry;
}

static void fx_watch_state_remove_wd(watch_state_t *state, int wd)
{
    watch_entry_t *entry;
    watch_entry_t *prev;

    entry = fx_watch_state_find_wd(state, wd, &prev);
    if (entry == NULL) return;

    if (prev == NULL) {
        state->watches = entry->next;
    } else {
        prev->next = entry->next;
    }
    fx_watch_entry_free(entry);
}
#endif /* __linux__ */

static int fx_dir_count_rec(const char *path, int *count)
{
    DIR *dir;
    struct dirent *entry;
    char child[PATH_MAX];
    int is_dir;
    int is_symlink_dir;
    int status;

    status = fx_path_kind(path, &is_dir, &is_symlink_dir);
    if (status != 0) return -1;
    if (!is_dir) return -1;

    (*count)++;
    if (is_symlink_dir) return 0;

    dir = opendir(path);
    if (dir == NULL) return -1;

    errno = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        if (fx_join_path(child, sizeof(child), path, entry->d_name) < 0) {
            closedir(dir);
            return -1;
        }
        status = fx_path_kind(child, &is_dir, &is_symlink_dir);
        if (status != 0) {
            closedir(dir);
            return -1;
        }
        if (!is_dir) {
            continue;
        }
        if (fx_dir_count_rec(child, count) != 0) {
            closedir(dir);
            return -1;
        }
    }

    if (errno != 0) {
        closedir(dir);
        return -1;
    }

    closedir(dir);
    return 0;
}

static int fx_dir_collect_rec(const char *path, char *dirs, int dir_len,
                              int max_dirs, int *count)
{
    DIR *dir;
    struct dirent *entry;
    char child[PATH_MAX];
    int is_dir;
    int is_symlink_dir;
    int status;
    char *slot;

    if (*count >= max_dirs) return -1;
    if (fx_path_kind(path, &is_dir, &is_symlink_dir) != 0) return -1;
    if (!is_dir) return -1;

    slot = dirs + ((size_t) *count) * (size_t) dir_len;
    memset(slot, 0, (size_t) dir_len);
    if (snprintf(slot, (size_t) dir_len, "%s", path) >= dir_len) {
        return -1;
    }
    (*count)++;
    if (is_symlink_dir) return 0;

    dir = opendir(path);
    if (dir == NULL) return -1;

    errno = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        if (fx_join_path(child, sizeof(child), path, entry->d_name) < 0) {
            closedir(dir);
            return -1;
        }
        status = fx_path_kind(child, &is_dir, &is_symlink_dir);
        if (status != 0) {
            closedir(dir);
            return -1;
        }
        if (!is_dir) {
            continue;
        }
        if (fx_dir_collect_rec(child, dirs, dir_len, max_dirs, count) != 0) {
            closedir(dir);
            return -1;
        }
    }

    if (errno != 0) {
        closedir(dir);
        return -1;
    }

    closedir(dir);
    return 0;
}

static int fx_file_count_rec(const char *path, int *count)
{
    DIR *dir;
    struct dirent *entry;
    char child[PATH_MAX];
    int is_dir;
    int is_symlink_dir;
    int status;

    status = fx_path_kind(path, &is_dir, &is_symlink_dir);
    if (status != 0) return -1;
    if (!is_dir) {
        (*count)++;
        return 0;
    }
    if (is_symlink_dir) return 0;

    dir = opendir(path);
    if (dir == NULL) return -1;

    errno = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        if (fx_join_path(child, sizeof(child), path, entry->d_name) < 0) {
            closedir(dir);
            return -1;
        }
        status = fx_path_kind(child, &is_dir, &is_symlink_dir);
        if (status != 0) {
            closedir(dir);
            return -1;
        }
        if (is_dir) {
            if (is_symlink_dir) {
                continue;
            }
            if (fx_file_count_rec(child, count) != 0) {
                closedir(dir);
                return -1;
            }
        } else {
            (*count)++;
        }
    }

    if (errno != 0) {
        closedir(dir);
        return -1;
    }

    closedir(dir);
    return 0;
}

static int fx_file_collect_rec(const char *path, char *files, int file_len,
                               int max_files, int *count)
{
    DIR *dir;
    struct dirent *entry;
    char child[PATH_MAX];
    int is_dir;
    int is_symlink_dir;
    int status;
    char *slot;

    if (*count >= max_files) return -1;
    status = fx_path_kind(path, &is_dir, &is_symlink_dir);
    if (status != 0) return -1;
    if (!is_dir) {
        slot = files + ((size_t) *count) * (size_t) file_len;
        memset(slot, 0, (size_t) file_len);
        if (snprintf(slot, (size_t) file_len, "%s", path) >= file_len) {
            return -1;
        }
        (*count)++;
        return 0;
    }
    if (is_symlink_dir) return 0;

    dir = opendir(path);
    if (dir == NULL) return -1;

    errno = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        if (fx_join_path(child, sizeof(child), path, entry->d_name) < 0) {
            closedir(dir);
            return -1;
        }
        status = fx_path_kind(child, &is_dir, &is_symlink_dir);
        if (status != 0) {
            closedir(dir);
            return -1;
        }
        if (is_dir) {
            if (is_symlink_dir) {
                continue;
            }
            if (fx_file_collect_rec(child, files, file_len, max_files, count) != 0) {
                closedir(dir);
                return -1;
            }
        } else {
            if (*count >= max_files) {
                closedir(dir);
                return -1;
            }
            slot = files + ((size_t) *count) * (size_t) file_len;
            memset(slot, 0, (size_t) file_len);
            if (snprintf(slot, (size_t) file_len, "%s", child) >= file_len) {
                closedir(dir);
                return -1;
            }
            (*count)++;
        }
    }

    if (errno != 0) {
        closedir(dir);
        return -1;
    }

    closedir(dir);
    return 0;
}

#ifdef __linux__
int fx_c_inotify_init(void)
{
    int fd;
    int flags;

    fd = inotify_init();
    if (fd < 0) return -1;

    flags = fcntl(fd, F_GETFL, 0);
    if (flags >= 0 && fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) {
        close(fd);
        return -1;
    }

    flags = fcntl(fd, F_GETFD, 0);
    if (flags >= 0 && fcntl(fd, F_SETFD, flags | FD_CLOEXEC) < 0) {
        close(fd);
        return -1;
    }

    if (fx_watch_state_add(fd) == NULL) {
        close(fd);
        return -1;
    }

    return fd;
}

int fx_c_inotify_add_watch(int fd, const char *path, int mask)
{
    char clean[PATH_MAX];
    int is_dir;
    int is_symlink_dir;
    int effective_mask;
    int wd;
    watch_state_t *state;

    state = fx_watch_state_find(fd);
    if (state == NULL) state = fx_watch_state_add(fd);
    if (state == NULL) return -1;

    fx_trim_path(path, clean, sizeof(clean));
    if (fx_inotify_force_enospc_once) {
        fx_inotify_force_enospc_once = 0;
        errno = ENOSPC;
        fprintf(stderr,
                "fx_watch: inotify watch limit reached for %s\n",
                clean);
        return -1;
    }
    effective_mask = mask;
    if (fx_path_kind(clean, &is_dir, &is_symlink_dir) == 0 &&
        is_symlink_dir && is_dir) {
        effective_mask |= IN_DONT_FOLLOW;
    }

    wd = inotify_add_watch(fd, clean, (uint32_t) effective_mask);
    if (wd < 0) {
        if (errno == ENOSPC) {
            fprintf(stderr,
                    "fx_watch: inotify watch limit reached for %s\n",
                    clean);
        }
        return -1;
    }

    fx_watch_state_store(state, wd, clean);
    return wd;
}

int fx_c_inotify_rm_watch(int fd, int wd)
{
    watch_state_t *state;
    int rc;

    state = fx_watch_state_find(fd);
    if (state == NULL) return -1;

    rc = inotify_rm_watch(fd, wd);
    if (rc < 0 && errno != EINVAL) return -1;

    fx_watch_state_remove_wd(state, wd);
    return 0;
}

int fx_c_inotify_close(int fd)
{
    int rc;

    if (fd < 0) return 0;
    fx_watch_state_remove_fd(fd);
    rc = close(fd);
    return rc;
}

int fx_c_inotify_poll(int fd, char *path_buf, int path_len,
                      int *event_type, int timeout_ms)
{
    struct pollfd pfd;
    ssize_t len;
    struct inotify_event *event;
    watch_state_t *state;
    watch_entry_t *entry;
    char full_path[PATH_MAX];
    int mapped_type;
    int got;
    size_t event_size;

    if (event_type != NULL) *event_type = 0;
    if (path_buf != NULL && path_len > 0) path_buf[0] = '\0';

    state = fx_watch_state_find(fd);
    if (state == NULL) return 0;

    for (;;) {
        if (state->pending_pos >= state->pending_len) {
            pfd.fd = fd;
            pfd.events = POLLIN;
            pfd.revents = 0;

            for (;;) {
                got = poll(&pfd, 1, timeout_ms);
                if (got < 0) {
                    if (errno == EINTR) continue;
                    return -1;
                }
                if (got == 0) return 0;
                break;
            }

            for (;;) {
                len = read(fd, state->pending, sizeof(state->pending));
                if (len < 0) {
                    if (errno == EINTR) continue;
                    if (errno == EAGAIN || errno == EWOULDBLOCK) return 0;
                    return -1;
                }
                if (len < (ssize_t) sizeof(struct inotify_event)) return 0;
                state->pending_len = (size_t) len;
                state->pending_pos = 0;
                break;
            }
        }

        event = (struct inotify_event *) (state->pending + state->pending_pos);
        event_size = sizeof(struct inotify_event) + (size_t) event->len;
        if (event_size == 0 || state->pending_pos + event_size > state->pending_len) {
            state->pending_pos = state->pending_len;
            continue;
        }
        state->pending_pos += event_size;
        if (state->pending_pos >= state->pending_len) {
            state->pending_pos = 0;
            state->pending_len = 0;
        }

        if ((event->mask & IN_IGNORED) != 0 ||
            (event->mask & IN_Q_OVERFLOW) != 0) {
            continue;
        }

        mapped_type = 0;
        if ((event->mask & (IN_DELETE | IN_DELETE_SELF |
                            IN_MOVED_FROM | IN_MOVE_SELF)) != 0) {
            mapped_type = 3;
        } else if ((event->mask & (IN_CREATE | IN_MOVED_TO)) != 0) {
            mapped_type = 2;
        } else if ((event->mask & IN_MODIFY) != 0) {
            mapped_type = 1;
        }
        if (mapped_type == 0) continue;

        entry = fx_watch_state_find_wd(state, event->wd, NULL);
        if (entry == NULL) continue;

        if (event->len > 0 && event->name[0] != '\0' &&
            (event->mask & (IN_DELETE_SELF | IN_MOVE_SELF)) == 0) {
            if (fx_join_path(full_path, sizeof(full_path), entry->path,
                             event->name) < 0) {
                continue;
            }
        } else {
            if (snprintf(full_path, sizeof(full_path), "%s", entry->path) >=
                (int) sizeof(full_path)) {
                continue;
            }
        }

        if (event_type != NULL) *event_type = mapped_type;
        if (path_buf != NULL && path_len > 0) {
            snprintf(path_buf, (size_t) path_len, "%s", full_path);
        }
        return 1;
    }
}

#elif defined(__APPLE__)
/* macOS/BSD file-watch backend via kqueue (EVFILT_VNODE).
   Mirrors the inotify contract: one watch per directory, child events by
   name, event types 1=modify 2=create 3=delete. Directory entry changes
   (create/delete/rename) are detected by re-scanning and diffing a snapshot;
   in-place file content writes are caught by a per-file O_EVTONLY fd, since a
   content write does not change the parent directory's entry list. */

typedef struct kq_child {
    char name[256];
    int is_dir;
    int file_fd;            /* O_EVTONLY fd for regular files; -1 otherwise */
    long long mtime;
    long long ino;
    int seen;
} kq_child_t;

typedef struct kq_watch {
    int wd;
    char *path;
    int dir_fd;             /* O_EVTONLY (O_SYMLINK for symlink dirs) */
    int is_symlink;
    kq_child_t *children;
    size_t n_children;
    size_t cap_children;
    struct kq_watch *next;
} kq_watch_t;

typedef struct kq_event {
    char path[PATH_MAX];
    int type;
    struct kq_event *next;
} kq_event_t;

typedef struct kq_state {
    int kq;
    int next_wd;
    kq_watch_t *watches;
    kq_event_t *head;
    kq_event_t *tail;
    struct kq_state *next;
} kq_state_t;

static kq_state_t *fx_kq_states = NULL;

static kq_state_t *fx_kq_find(int kq)
{
    kq_state_t *s;
    for (s = fx_kq_states; s != NULL; s = s->next) {
        if (s->kq == kq) return s;
    }
    return NULL;
}

static void fx_kq_enqueue(kq_state_t *s, const char *path, int type)
{
    kq_event_t *e = (kq_event_t *) calloc(1, sizeof(*e));
    if (e == NULL) return;
    snprintf(e->path, sizeof(e->path), "%s", path);
    e->type = type;
    e->next = NULL;
    if (s->tail != NULL) s->tail->next = e; else s->head = e;
    s->tail = e;
}

static int fx_kq_dequeue(kq_state_t *s, char *buf, int len, int *type)
{
    kq_event_t *e = s->head;
    if (e == NULL) return 0;
    s->head = e->next;
    if (s->head == NULL) s->tail = NULL;
    if (buf != NULL && len > 0) snprintf(buf, (size_t) len, "%s", e->path);
    if (type != NULL) *type = e->type;
    free(e);
    return 1;
}

static kq_child_t *fx_kq_child_find(kq_watch_t *w, const char *name)
{
    size_t i;
    for (i = 0; i < w->n_children; i++) {
        if (strcmp(w->children[i].name, name) == 0) return &w->children[i];
    }
    return NULL;
}

static kq_child_t *fx_kq_child_add(kq_watch_t *w)
{
    kq_child_t *c;
    if (w->n_children >= w->cap_children) {
        size_t ncap = w->cap_children ? w->cap_children * 2 : 8;
        kq_child_t *nc = (kq_child_t *) realloc(w->children, ncap * sizeof(*nc));
        if (nc == NULL) return NULL;
        w->children = nc;
        w->cap_children = ncap;
    }
    c = &w->children[w->n_children++];
    memset(c, 0, sizeof(*c));
    c->file_fd = -1;
    return c;
}

static void fx_kq_child_remove_at(kq_watch_t *w, size_t idx)
{
    if (idx >= w->n_children) return;
    if (w->children[idx].file_fd >= 0) close(w->children[idx].file_fd);
    w->children[idx] = w->children[w->n_children - 1];
    w->n_children--;
}

static void fx_kq_register(int kq, int fd, void *udata)
{
    struct kevent kev;
    EV_SET(&kev, (uintptr_t) fd, EVFILT_VNODE, EV_ADD | EV_CLEAR,
           NOTE_WRITE | NOTE_DELETE | NOTE_RENAME | NOTE_EXTEND, 0, udata);
    kevent(kq, &kev, 1, NULL, 0, NULL);
}

/* Re-scan a watched directory and reconcile against its snapshot. When emit
   is set, queue create/delete/modify events for the differences. */
static void fx_kq_sync_children(kq_state_t *s, kq_watch_t *w, int emit)
{
    DIR *dir;
    struct dirent *de;
    char child_path[PATH_MAX];
    struct stat st;
    size_t i;

    for (i = 0; i < w->n_children; i++) w->children[i].seen = 0;

    dir = opendir(w->path);
    if (dir != NULL) {
        while ((de = readdir(dir)) != NULL) {
            int is_dir;
            int is_reg;
            long long mtime;
            long long ino;
            kq_child_t *c;

            if (strcmp(de->d_name, ".") == 0 ||
                strcmp(de->d_name, "..") == 0) {
                continue;
            }
            if (fx_join_path(child_path, sizeof(child_path), w->path,
                             de->d_name) < 0) {
                continue;
            }
            if (lstat(child_path, &st) != 0) continue;
            is_dir = S_ISDIR(st.st_mode);
            is_reg = S_ISREG(st.st_mode);
            mtime = (long long) st.st_mtime;
            ino = (long long) st.st_ino;

            c = fx_kq_child_find(w, de->d_name);
            if (c == NULL) {
                c = fx_kq_child_add(w);
                if (c == NULL) continue;
                snprintf(c->name, sizeof(c->name), "%s", de->d_name);
                c->is_dir = is_dir;
                c->mtime = mtime;
                c->ino = ino;
                c->file_fd = -1;
                c->seen = 1;
                if (is_reg) {
                    c->file_fd = open(child_path, O_EVTONLY);
                    if (c->file_fd >= 0) fx_kq_register(s->kq, c->file_fd, w);
                }
                if (emit) fx_kq_enqueue(s, child_path, 2);
            } else {
                c->seen = 1;
                if (is_reg && (mtime != c->mtime || ino != c->ino)) {
                    if (ino != c->ino) {
                        if (c->file_fd >= 0) close(c->file_fd);
                        c->file_fd = open(child_path, O_EVTONLY);
                        if (c->file_fd >= 0) {
                            fx_kq_register(s->kq, c->file_fd, w);
                        }
                    }
                    c->mtime = mtime;
                    c->ino = ino;
                    if (emit) fx_kq_enqueue(s, child_path, 1);
                } else {
                    c->mtime = mtime;
                    c->ino = ino;
                }
            }
        }
        closedir(dir);
    }

    for (i = w->n_children; i > 0; i--) {
        size_t idx = i - 1;
        if (!w->children[idx].seen) {
            if (emit) {
                char gone[PATH_MAX];
                if (fx_join_path(gone, sizeof(gone), w->path,
                                 w->children[idx].name) >= 0) {
                    fx_kq_enqueue(s, gone, 3);
                }
            }
            fx_kq_child_remove_at(w, idx);
        }
    }
}

int fx_c_inotify_init(void)
{
    int kq;
    int flags;
    kq_state_t *s;

    kq = kqueue();
    if (kq < 0) return -1;

    flags = fcntl(kq, F_GETFD, 0);
    if (flags >= 0) fcntl(kq, F_SETFD, flags | FD_CLOEXEC);

    s = (kq_state_t *) calloc(1, sizeof(*s));
    if (s == NULL) {
        close(kq);
        return -1;
    }
    s->kq = kq;
    s->next_wd = 1;
    s->watches = NULL;
    s->head = NULL;
    s->tail = NULL;
    s->next = fx_kq_states;
    fx_kq_states = s;
    return kq;
}

int fx_c_inotify_add_watch(int fd, const char *path, int mask)
{
    char clean[PATH_MAX];
    kq_state_t *s;
    kq_watch_t *w;
    int is_dir;
    int is_symlink_dir;

    (void) mask;
    s = fx_kq_find(fd);
    if (s == NULL) return -1;

    fx_trim_path(path, clean, sizeof(clean));

    if (fx_inotify_force_enospc_once) {
        fx_inotify_force_enospc_once = 0;
        errno = ENOSPC;
        fprintf(stderr,
                "fx_watch: inotify watch limit reached for %s\n", clean);
        return -1;
    }

    if (fx_path_kind(clean, &is_dir, &is_symlink_dir) != 0) return -1;
    if (!is_dir) return -1;

    w = (kq_watch_t *) calloc(1, sizeof(*w));
    if (w == NULL) return -1;
    w->path = strdup(clean);
    if (w->path == NULL) {
        free(w);
        return -1;
    }
    w->wd = s->next_wd++;
    w->dir_fd = -1;
    w->is_symlink = is_symlink_dir;
    w->children = NULL;
    w->n_children = 0;
    w->cap_children = 0;

    if (is_symlink_dir) {
        /* watch the symlink itself, never its target (matches IN_DONT_FOLLOW) */
        w->dir_fd = open(clean, O_EVTONLY | O_SYMLINK);
        if (w->dir_fd >= 0) fx_kq_register(s->kq, w->dir_fd, w);
    } else {
        w->dir_fd = open(clean, O_EVTONLY);
        if (w->dir_fd < 0) {
            free(w->path);
            free(w);
            return -1;
        }
        fx_kq_register(s->kq, w->dir_fd, w);
        fx_kq_sync_children(s, w, 0);
    }

    w->next = s->watches;
    s->watches = w;
    return w->wd;
}

int fx_c_inotify_rm_watch(int fd, int wd)
{
    kq_state_t *s;
    kq_watch_t *w;
    kq_watch_t *prev;
    size_t i;

    s = fx_kq_find(fd);
    if (s == NULL) return -1;

    prev = NULL;
    for (w = s->watches; w != NULL; prev = w, w = w->next) {
        if (w->wd != wd) continue;
        if (prev != NULL) prev->next = w->next; else s->watches = w->next;
        for (i = 0; i < w->n_children; i++) {
            if (w->children[i].file_fd >= 0) close(w->children[i].file_fd);
        }
        free(w->children);
        if (w->dir_fd >= 0) close(w->dir_fd);
        free(w->path);
        free(w);
        return 0;
    }
    return 0;
}

int fx_c_inotify_close(int fd)
{
    kq_state_t *s;
    kq_state_t *prev;
    kq_watch_t *w;
    kq_watch_t *wn;
    kq_event_t *e;
    kq_event_t *en;
    size_t i;

    if (fd < 0) return 0;

    prev = NULL;
    for (s = fx_kq_states; s != NULL; prev = s, s = s->next) {
        if (s->kq != fd) continue;
        if (prev != NULL) prev->next = s->next; else fx_kq_states = s->next;
        for (w = s->watches; w != NULL; w = wn) {
            wn = w->next;
            for (i = 0; i < w->n_children; i++) {
                if (w->children[i].file_fd >= 0) close(w->children[i].file_fd);
            }
            free(w->children);
            if (w->dir_fd >= 0) close(w->dir_fd);
            free(w->path);
            free(w);
        }
        for (e = s->head; e != NULL; e = en) {
            en = e->next;
            free(e);
        }
        close(s->kq);
        free(s);
        return 0;
    }
    return close(fd);
}

int fx_c_inotify_poll(int fd, char *path_buf, int path_len,
                      int *event_type, int timeout_ms)
{
    kq_state_t *s;
    struct kevent evs[64];
    struct timespec ts;
    int n;
    int i;

    if (event_type != NULL) *event_type = 0;
    if (path_buf != NULL && path_len > 0) path_buf[0] = '\0';

    s = fx_kq_find(fd);
    if (s == NULL) return 0;

    if (fx_kq_dequeue(s, path_buf, path_len, event_type)) return 1;

    ts.tv_sec = timeout_ms / 1000;
    ts.tv_nsec = (long) (timeout_ms % 1000) * 1000000L;

    n = kevent(s->kq, NULL, 0, evs, 64, &ts);
    if (n < 0) {
        if (errno == EINTR) return 0;
        return -1;
    }
    if (n == 0) return 0;

    for (i = 0; i < n; i++) {
        kq_watch_t *w = (kq_watch_t *) evs[i].udata;
        if (w == NULL) continue;
        if (evs[i].flags & EV_ERROR) continue;

        if ((int) evs[i].ident == w->dir_fd) {
            fx_kq_sync_children(s, w, 1);
        } else if (evs[i].fflags & (NOTE_WRITE | NOTE_EXTEND)) {
            size_t k;
            for (k = 0; k < w->n_children; k++) {
                if (w->children[k].file_fd == (int) evs[i].ident) {
                    char p[PATH_MAX];
                    if (fx_join_path(p, sizeof(p), w->path,
                                     w->children[k].name) >= 0) {
                        fx_kq_enqueue(s, p, 1);
                    }
                    break;
                }
            }
        }
    }

    if (fx_kq_dequeue(s, path_buf, path_len, event_type)) return 1;
    return 0;
}

#else /* other platforms: stubs without a file-watch backend */
void fx_c_inotify_test_force_enospc_once(void) { }
int fx_c_inotify_init(void) { return -1; }
int fx_c_inotify_add_watch(int fd, const char *path, int mask)
    { (void)fd; (void)path; (void)mask; return -1; }
int fx_c_inotify_rm_watch(int fd, int wd) { (void)fd; (void)wd; return -1; }
int fx_c_inotify_close(int fd) { (void)fd; return -1; }
int fx_c_inotify_poll(int fd, char *path_buf, int path_len,
    int *event_type, int timeout_ms)
    { (void)fd; (void)path_buf; (void)path_len; (void)event_type;
      (void)timeout_ms; return -1; }
#endif /* __linux__ */

int fx_c_count_dirs(const char *root, int *n_dirs)
{
    int count;

    if (n_dirs == NULL) return -1;
    count = 0;
    if (fx_dir_count_rec(root, &count) != 0) return -1;
    *n_dirs = count;
    return 0;
}

int fx_c_collect_dirs(const char *root, char *dirs, int dir_len,
                      int *n_dirs, int max_dirs)
{
    int count;

    if (n_dirs == NULL || dirs == NULL || dir_len <= 0 || max_dirs <= 0) {
        return -1;
    }

    count = 0;
    if (fx_dir_collect_rec(root, dirs, dir_len, max_dirs, &count) != 0) {
        return -1;
    }
    *n_dirs = count;
    return 0;
}

int fx_c_count_files(const char *root, int *n_files)
{
    int count;

    if (n_files == NULL) return -1;
    count = 0;
    if (fx_file_count_rec(root, &count) != 0) return -1;
    *n_files = count;
    return 0;
}

int fx_c_collect_files(const char *root, char *files, int file_len,
                       int *n_files, int max_files)
{
    int count;

    if (n_files == NULL || files == NULL || file_len <= 0 || max_files <= 0) {
        return -1;
    }

    count = 0;
    if (fx_file_collect_rec(root, files, file_len, max_files, &count) != 0) {
        return -1;
    }
    *n_files = count;
    return 0;
}

int fx_c_path_is_dir(const char *path)
{
    int is_dir;
    int is_symlink_dir;

    if (fx_path_kind(path, &is_dir, &is_symlink_dir) != 0) return 0;
    return is_dir ? 1 : 0;
}
