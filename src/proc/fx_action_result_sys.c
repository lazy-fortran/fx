#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static int test_pause_phase;
static char test_pause_ready[PATH_MAX];
static char test_pause_release[PATH_MAX];

static int valid_id(const char *id)
{
    if (!id || strlen(id) != 64) return 0;
    for (int i = 0; i < 64; ++i)
        if (!((id[i] >= '0' && id[i] <= '9') ||
              (id[i] >= 'a' && id[i] <= 'f'))) return 0;
    return 1;
}

static int sync_directory(const char *path)
{
    int fd = open(path, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
    if (fd < 0) return -1;
    int rc = fsync(fd);
    close(fd);
    return rc;
}

static int ensure_directory(const char *path)
{
    char copy[PATH_MAX];
    size_t n = strlen(path);
    if (n == 0 || n >= sizeof(copy)) return -1;
    memcpy(copy, path, n + 1);
    for (char *p = copy + 1; ; ++p) {
        if (*p != '/' && *p != '\0') continue;
        char saved = *p;
        *p = '\0';
        int created = mkdir(copy, 0755) == 0;
        if (!created && errno != EEXIST) return -1;
        struct stat st;
        if (stat(copy, &st) != 0 || !S_ISDIR(st.st_mode)) return -1;
        if (created) {
            char parent[PATH_MAX];
            char *last = strrchr(copy, '/');
            size_t parent_len = last ? (size_t)(last - copy) : 0;
            if (parent_len == 0) {
                strcpy(parent, "/");
            } else {
                if (parent_len >= sizeof(parent)) return -1;
                memcpy(parent, copy, parent_len);
                parent[parent_len] = '\0';
            }
            if (sync_directory(parent) != 0) return -1;
        }
        if (saved == '\0') break;
        *p = saved;
    }
    return 0;
}

static int action_dir(const char *root, const char *id, char *out, size_t cap)
{
    if (!valid_id(id)) return -1;
    int n = snprintf(out, cap, "%s/actions/sha256/%.2s", root, id);
    if (n < 0 || (size_t)n >= cap) return -1;
    return ensure_directory(out);
}

int fx_action_result_lock(const char *root, const char *id)
{
    char dir[PATH_MAX], path[PATH_MAX];
    if (action_dir(root, id, dir, sizeof(dir)) != 0) return -1;
    (void)sync_directory(dir);
    int n = snprintf(path, sizeof(path), "%s/%s.lock", dir, id);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    int fd = open(path, O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (fd < 0) return -1;
    while (flock(fd, LOCK_EX) != 0) {
        if (errno != EINTR) { close(fd); return -1; }
    }
    return fd;
}

int fx_action_result_unlock(int fd)
{
    int rc = flock(fd, LOCK_UN);
    if (close(fd) != 0) rc = -1;
    return rc;
}

int fx_action_result_exists(const char *root, const char *id)
{
    char dir[PATH_MAX];
    if (!valid_id(id)) return -1;
    int n = snprintf(dir, sizeof(dir), "%s/actions/sha256/%.2s", root, id);
    if (n < 0 || (size_t)n >= sizeof(dir)) return -1;
    int dfd = open(dir, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (dfd < 0) return errno == ENOENT ? 1 : -1;
    int fd = openat(dfd, id, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    int saved = errno;
    close(dfd);
    if (fd < 0) return saved == ENOENT ? 1 : -1;
    struct stat st;
    int rc = fstat(fd, &st);
    close(fd);
    return rc == 0 && S_ISREG(st.st_mode) ? 0 : -1;
}

static int action_record(const char *root, const char *id, char *dir,
                         size_t dir_cap, char *name, size_t name_cap)
{
    int n;
    if (action_dir(root, id, dir, dir_cap) != 0) return -1;
    n = snprintf(name, name_cap, "%s", id);
    return n < 0 || (size_t)n >= name_cap ? -1 : 0;
}

int fx_action_result_read(const char *root, const char *id, char *bytes,
                          int capacity, int *count)
{
    char dir[PATH_MAX], name[80], path[PATH_MAX];
    *count = 0;
    if (action_record(root, id, dir, sizeof(dir), name, sizeof(name)) != 0)
        return -1;
    int n = snprintf(path, sizeof(path), "%s/%s", dir, name);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno == ENOENT ? 1 : -1;
    struct stat st;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode) ||
        st.st_size < 0 || st.st_size > capacity) { close(fd); return -1; }
    int total = 0;
    while (total < st.st_size) {
        ssize_t got = read(fd, bytes + total, (size_t)(st.st_size - total));
        if (got < 0 && errno == EINTR) continue;
        if (got <= 0) { close(fd); return -1; }
        total += (int)got;
    }
    close(fd);
    *count = total;
    return 0;
}

static void publication_pause(int phase)
{
    if (test_pause_phase != phase || !test_pause_ready[0] ||
        !test_pause_release[0]) return;
    int fd = open(test_pause_ready, O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, 0600);
    if (fd < 0) return;
    (void)write(fd, "ready\n", 6);
    (void)fsync(fd);
    close(fd);
    struct timespec delay = {0, 10000000};
    for (int i = 0; i < 30000; ++i) {
        if (access(test_pause_release, F_OK) == 0) break;
        (void)nanosleep(&delay, NULL);
    }
}

int fx_action_result_write(const char *root, const char *id,
                           const char *bytes, int count)
{
    char dir[PATH_MAX], name[80], temp[PATH_MAX];
    static unsigned long sequence;
    if (count < 1 || action_record(root, id, dir, sizeof(dir),
                                    name, sizeof(name)) != 0) return -1;
    int dfd = open(dir, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (dfd < 0) return -1;
    unsigned long ticket = __sync_add_and_fetch(&sequence, 1);
    int n = snprintf(temp, sizeof(temp), ".tmp.%ld.%lu", (long)getpid(), ticket);
    if (n < 0 || (size_t)n >= sizeof(temp)) { close(dfd); return -1; }
    int fd = openat(dfd, temp, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC |
                    O_NOFOLLOW, 0600);
    if (fd < 0) { close(dfd); return -1; }
    int total = 0;
    while (total < count) {
        ssize_t wrote = write(fd, bytes + total, (size_t)(count - total));
        if (wrote < 0 && errno == EINTR) continue;
        if (wrote <= 0) { close(fd); unlinkat(dfd, temp, 0); close(dfd); return -1; }
        total += (int)wrote;
    }
    if (fsync(fd) != 0) { close(fd); unlinkat(dfd, temp, 0); close(dfd); return -1; }
    close(fd);
    publication_pause(1);
    if (renameat(dfd, temp, dfd, name) != 0) {
        unlinkat(dfd, temp, 0); close(dfd); return -1;
    }
    publication_pause(2);
    int rc = fsync(dfd);
    close(dfd);
    return rc;
}

int fx_action_result_file_mode(const char *path, int *mode)
{
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return -1;
    struct stat st;
    int rc = fstat(fd, &st);
    close(fd);
    if (rc != 0 || !S_ISREG(st.st_mode)) return -1;
    *mode = (int)(st.st_mode & 0777);
    return 0;
}

int fx_action_result_temp_path(const char *destination, char *output, int capacity)
{
    static unsigned long sequence;
    unsigned long ticket = __sync_add_and_fetch(&sequence, 1);
    const char *slash = strrchr(destination, '/');
    int n;
    if (!slash) {
        n = snprintf(output, (size_t)capacity, "./.fx-result-%ld-%lu",
                     (long)getpid(), ticket);
    } else {
        size_t parent = (size_t)(slash - destination);
        n = snprintf(output, (size_t)capacity, "%.*s/.fx-result-%ld-%lu",
                     (int)parent, destination, (long)getpid(), ticket);
    }
    return n < 0 || n >= capacity ? -1 : 0;
}

int fx_action_result_replace(const char *source, const char *destination)
{
    char parent[PATH_MAX];
    const char *slash = strrchr(destination, '/');
    size_t n = slash ? (size_t)(slash - destination) : 1;
    if (n >= sizeof(parent)) return -1;
    if (slash && n > 0) memcpy(parent, destination, n);
    if (!slash) parent[0] = '.';
    if (slash && n == 0) {
        parent[0] = '/';
        n = 1;
    }
    parent[n] = '\0';
    if (rename(source, destination) != 0) return -1;
    return sync_directory(parent);
}

int fx_action_result_unlink(const char *path)
{
    if (unlink(path) == 0 || errno == ENOENT) return 0;
    return -1;
}

int fx_action_result_test_configure(int phase, const char *ready,
                                    const char *release)
{
    if (strlen(ready) >= sizeof(test_pause_ready) ||
        strlen(release) >= sizeof(test_pause_release)) return -1;
    strcpy(test_pause_ready, ready);
    strcpy(test_pause_release, release);
    test_pause_phase = phase;
    return 0;
}

int fx_action_result_test_barrier(const char *ready, const char *release)
{
    int fd = open(ready, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC, 0600);
    if (fd < 0) return -1;
    if (write(fd, "ready\n", 6) != 6 || fsync(fd) != 0) {
        close(fd);
        return -1;
    }
    close(fd);
    struct timespec delay = {0, 10000000};
    for (int i = 0; i < 30000; ++i) {
        if (access(release, F_OK) == 0) return 0;
        (void)nanosleep(&delay, NULL);
    }
    return -1;
}
