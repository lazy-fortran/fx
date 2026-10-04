#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <time.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

struct source_paths {
    char **items;
    size_t count;
    size_t capacity;
};

static int source_path_compare(const void *left, const void *right)
{
    const char *const *a = left;
    const char *const *b = right;
    return strcmp(*a, *b);
}

static void source_paths_free(struct source_paths *paths)
{
    size_t i;
    for (i = 0; i < paths->count; ++i) free(paths->items[i]);
    free(paths->items);
    paths->items = NULL;
    paths->count = 0;
    paths->capacity = 0;
}

static int source_paths_add(struct source_paths *paths, const char *path)
{
    char **next;
    size_t capacity;

    if (paths->count == paths->capacity) {
        capacity = paths->capacity == 0 ? 64 : paths->capacity * 2;
        next = realloc(paths->items, capacity * sizeof(*next));
        if (next == NULL) return -1;
        paths->items = next;
        paths->capacity = capacity;
    }
    paths->items[paths->count] = strdup(path);
    if (paths->items[paths->count] == NULL) return -1;
    paths->count++;
    return 0;
}

static int source_paths_scan(const char *root, struct source_paths *paths)
{
    DIR *directory;
    struct dirent *entry;
    struct stat info;
    char child[PATH_MAX];
    int result = 0;

    directory = opendir(root);
    if (directory == NULL) return -1;
    errno = 0;
    while ((entry = readdir(directory)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0 ||
            strcmp(entry->d_name, ".git") == 0 ||
            strcmp(entry->d_name, "build") == 0) {
            continue;
        }
        if (snprintf(child, sizeof(child), "%s/%s", root, entry->d_name) >=
            (int)sizeof(child)) {
            result = -1;
            break;
        }
        if (lstat(child, &info) != 0) {
            result = -1;
            break;
        }
        if (S_ISLNK(info.st_mode)) {
            if (stat(child, &info) != 0) {
                errno = 0;
                continue;
            }
            if (!S_ISREG(info.st_mode)) {
                errno = 0;
                continue;
            }
        } else if (S_ISDIR(info.st_mode)) {
            if (source_paths_scan(child, paths) != 0) {
                result = -1;
                break;
            }
            errno = 0;
            continue;
        } else if (!S_ISREG(info.st_mode)) {
            errno = 0;
            continue;
        }
        if (source_paths_add(paths, child) != 0) {
            result = -1;
            break;
        }
        errno = 0;
    }
    if (entry == NULL && errno != 0) result = -1;
    if (closedir(directory) != 0) result = -1;
    return result;
}

static int source_paths_read(const char *root, struct source_paths *paths)
{
    paths->items = NULL;
    paths->count = 0;
    paths->capacity = 0;
    if (source_paths_scan(root, paths) != 0) {
        source_paths_free(paths);
        return -1;
    }
    if (paths->count > 1) {
        qsort(paths->items, paths->count, sizeof(*paths->items),
              source_path_compare);
    }
    return 0;
}

int fx_test_fs_source_count(const char *root, int *count)
{
    struct source_paths paths;
    if (root == NULL || count == NULL || source_paths_read(root, &paths) != 0)
        return -1;
    if (paths.count > 2147483647U) {
        source_paths_free(&paths);
        return -1;
    }
    *count = (int)paths.count;
    source_paths_free(&paths);
    return 0;
}

int fx_test_fs_source_collect(const char *root, char *output, int slot_len,
                              int max_paths, int *count)
{
    struct source_paths paths;
    size_t i;
    if (root == NULL || output == NULL || count == NULL || slot_len <= 0 ||
        max_paths < 0 || source_paths_read(root, &paths) != 0) return -1;
    if (paths.count > (size_t)max_paths) {
        source_paths_free(&paths);
        return -1;
    }
    for (i = 0; i < paths.count; ++i) {
        char *slot = output + i * (size_t)slot_len;
        if (snprintf(slot, (size_t)slot_len, "%s", paths.items[i]) >= slot_len) {
            source_paths_free(&paths);
            return -1;
        }
    }
    *count = (int)paths.count;
    source_paths_free(&paths);
    return 0;
}

int fx_test_fs_lock(const char *path)
{
    int fd = open(path, O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (fd < 0) return -1;
    struct timespec delay = {0, 10000000};
    for (int i = 0; i < 1500; ++i) {
        if (flock(fd, LOCK_EX | LOCK_NB) == 0) return fd;
        if (errno != EWOULDBLOCK && errno != EINTR) break;
        (void)nanosleep(&delay, NULL);
    }
    close(fd);
    return -1;
}

int fx_test_fs_unlock(int fd)
{
    if (fd < 0) return -1;
    int result = flock(fd, LOCK_UN);
    if (close(fd) != 0) result = -1;
    return result;
}

int fx_test_fs_descriptor_count(void)
{
    DIR *directory = opendir("/dev/fd");
    struct dirent *entry;
    int count = 0;
    if (directory == NULL) return -1;
    while ((entry = readdir(directory)) != NULL) {
        char *end;
        long fd = strtol(entry->d_name, &end, 10);
        if (*end || fd < 0 || fd > INT_MAX || fd == dirfd(directory)) continue;
        if (fcntl((int)fd, F_GETFD) >= 0) ++count;
    }
    closedir(directory);
    return count;
}

static int safe_fixture_root(const char *path)
{
    static const char *const prefixes[] = {"/tmp/", "/var/tmp/"};
    size_t i;
    if (path == NULL || path[0] == '\0') return 0;
    for (i = 0; i < sizeof(prefixes) / sizeof(prefixes[0]); ++i) {
        size_t n = strlen(prefixes[i]);
        if (strncmp(path, prefixes[i], n) == 0 && path[n] != '\0') {
            const char *part = path + n;
            while (*part != '\0') {
                const char *end = strchr(part, '/');
                size_t len = end == NULL ? strlen(part) : (size_t)(end - part);
                if (len == 0 || (len == 1 && part[0] == '.') ||
                    (len == 2 && part[0] == '.' && part[1] == '.')) return 0;
                if (end == NULL) return 1;
                part = end + 1;
            }
        }
    }
    return 0;
}

static int mkdir_p_path(const char *path)
{
    char *copy;
    char *p;
    struct stat st;
    size_t n;
    if (path == NULL || path[0] == '\0') { errno = EINVAL; return -1; }
    copy = strdup(path);
    if (copy == NULL) return -1;
    n = strlen(copy);
    while (n > 1 && copy[n - 1] == '/') copy[--n] = '\0';
    for (p = copy + (copy[0] == '/' ? 1 : 0); ; ++p) {
        int at_end;
        if (*p != '/' && *p != '\0') continue;
        at_end = *p == '\0';
        *p = '\0';
        if (mkdir(copy, 0777) != 0 && errno != EEXIST) {
            free(copy);
            return -1;
        }
        if (stat(copy, &st) != 0 || !S_ISDIR(st.st_mode)) {
            errno = ENOTDIR;
            free(copy);
            return -1;
        }
        if (at_end) break;
        *p = '/';
    }
    free(copy);
    return 0;
}

int fx_test_fs_mkdir_p(const char *path)
{
    return mkdir_p_path(path);
}

static int remove_entry(const char *path)
{
    struct stat st;
    DIR *dir;
    struct dirent *entry;
    int result = 0;
    if (lstat(path, &st) != 0) return errno == ENOENT ? 0 : -1;
    if (!S_ISDIR(st.st_mode)) return unlink(path);
    dir = opendir(path);
    if (dir == NULL) return -1;
    errno = 0;
    while ((entry = readdir(dir)) != NULL) {
        char *child;
        size_t n;
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;
        n = strlen(path) + strlen(entry->d_name) + 2;
        child = (char *)malloc(n);
        if (child == NULL) { result = -1; break; }
        (void)snprintf(child, n, "%s/%s", path, entry->d_name);
        if (remove_entry(child) != 0) result = -1;
        free(child);
        if (result != 0) break;
        errno = 0;
    }
    if (entry == NULL && errno != 0) result = -1;
    if (closedir(dir) != 0) result = -1;
    if (result == 0 && rmdir(path) != 0) result = -1;
    return result;
}

int fx_test_fs_remove_tree(const char *path)
{
    if (!safe_fixture_root(path)) { errno = EINVAL; return -1; }
    return remove_entry(path);
}

int fx_test_fs_rename(const char *source, const char *destination)
{
    if (!safe_fixture_root(source) || !safe_fixture_root(destination)) {
        errno = EINVAL;
        return -1;
    }
    return rename(source, destination);
}

int fx_test_fs_symlink(const char *target, const char *link_path)
{
    if (target == NULL || !safe_fixture_root(link_path)) {
        errno = EINVAL;
        return -1;
    }
    return symlink(target, link_path);
}

int fx_test_fs_chmod(const char *path, int mode)
{
    if (!safe_fixture_root(path)) { errno = EINVAL; return -1; }
    return chmod(path, (mode_t)mode);
}

int fx_test_fs_sleep_ms(int64_t milliseconds)
{
    struct timespec now;
    struct timespec deadline;
    struct timespec request;
    if (milliseconds < 0) { errno = EINVAL; return -1; }
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
    deadline.tv_sec = now.tv_sec + (time_t)(milliseconds / 1000);
    deadline.tv_nsec = now.tv_nsec + (long)(milliseconds % 1000) * 1000000L;
    if (deadline.tv_nsec >= 1000000000L) {
        deadline.tv_sec++;
        deadline.tv_nsec -= 1000000000L;
    }
    for (;;) {
        request.tv_sec = deadline.tv_sec - now.tv_sec;
        request.tv_nsec = deadline.tv_nsec - now.tv_nsec;
        if (request.tv_nsec < 0) {
            request.tv_sec--;
            request.tv_nsec += 1000000000L;
        }
        if (request.tv_sec < 0 ||
            (request.tv_sec == 0 && request.tv_nsec == 0)) return 0;
        if (nanosleep(&request, NULL) != 0 && errno != EINTR) return -1;
        if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
    }
}
