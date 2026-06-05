#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/inotify.h>
#include <dirent.h>
#include <poll.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <limits.h>
#include <time.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static int fx_inotify_force_enospc_once = 0;

/*
 * fx_sys.c: C implementations for fx_proc.f90 Fortran interfaces.
 * Provides process execution, directory scanning, file I/O, and
 * signal handling via POSIX APIs.
 */

/* Fork/execvp with stdout+stderr capture via pipes. */
int fx_c_exec(const char *argv, int n_argv,
              char *stdout_buf, int *stdout_len,
              char *stderr_buf, int *stderr_len)
{
    (void)argv;
    (void)n_argv;
    *stdout_len = 0;
    *stderr_len = 0;
    stdout_buf[0] = '\0';
    stderr_buf[0] = '\0';
    return -1; /* not implemented */
}

/* Fork/execvp without output capture. Returns exit code. */
int fx_c_exec_silent(const char *argv, int n_argv)
{
    (void)argv;
    (void)n_argv;
    return -1; /* not implemented */
}

/*
 * Recursive directory scan.
 * Walks root, collects files matching given extensions.
 * Skips .git, build, node_modules directories.
 * Returns sorted file list.
 */
/* !$omp parallel: future parallelism for large directory trees */
int fx_c_scan_dir(const char *root, const char *extensions, int n_ext,
                  char *files, int *n_files, int max_files)
{
    (void)root;
    (void)extensions;
    (void)n_ext;
    (void)files;
    (void)max_files;
    *n_files = 0;
    return -1; /* not implemented */
}

/* Read entire file into buffer. Returns 0 on success, -1 on error. */
int fx_c_file_read(const char *path, char *content, int *n_bytes)
{
    (void)path;
    (void)content;
    *n_bytes = 0;
    return -1; /* not implemented */
}

/* Write buffer to file. Returns 0 on success, -1 on error. */
int fx_c_file_write(const char *path, const char *content, int n_bytes)
{
    (void)path;
    (void)content;
    (void)n_bytes;
    return -1; /* not implemented */
}

/* Create a temp file with given prefix. Returns path and length. */
void fx_c_tmpfile(const char *prefix, char *path, int *path_len)
{
    (void)prefix;
    path[0] = '\0';
    *path_len = 0;
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

void fx_c_inotify_test_force_enospc_once(void)
{
    fx_inotify_force_enospc_once = 1;
}

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
            break;
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

long long fx_c_unix_time(void)
{
    return (long long) time(NULL);
}

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
