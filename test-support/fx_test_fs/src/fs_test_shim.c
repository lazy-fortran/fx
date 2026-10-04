#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <time.h>
#include <unistd.h>

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

int fx_test_fs_lock_directory(const char *path)
{
    int flags = O_RDONLY | O_CLOEXEC | O_NOFOLLOW;
    int fd;
#ifdef O_DIRECTORY
    flags |= O_DIRECTORY;
#endif
    fd = open(path, flags);
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

int fx_test_fs_temp_root(char *out, int cap)
{
    char *path = realpath("/var/tmp", NULL);
    int length;
    if (!path) return -1;
    length = snprintf(out, (size_t)cap, "%s", path);
    free(path);
    return length >= 0 && length < cap ? 0 : -1;
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
