#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#else
#define _DEFAULT_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#include <dirent.h>
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

static int path_at(const char *root, const char *name, char *out, size_t cap)
{
    int n = snprintf(out, cap, "%s/.fx-metadata/%s", root, name);
    return n < 0 || (size_t)n >= cap ? -1 : 0;
}

static int valid_key(const char *name)
{
    if (strlen(name) != 64) return 0;
    for (int i = 0; i < 64; ++i)
        if (!((name[i] >= '0' && name[i] <= '9') ||
              (name[i] >= 'a' && name[i] <= 'f'))) return 0;
    return 1;
}

/* A separate nonblocking owner lock avoids waiting on GC or another scanner. */
int fx_action_maintenance_begin(const char *root)
{
    char path[PATH_MAX];
    if (path_at(root, "action-maintenance.lock", path, sizeof(path)) != 0)
        return -1;
    int fd = open(path, O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (fd < 0) return -1;
    if (flock(fd, LOCK_EX | LOCK_NB) != 0) {
        int busy = errno == EWOULDBLOCK || errno == EAGAIN;
        close(fd);
        return busy ? -2 : -1;
    }
    return fd;
}

int fx_action_maintenance_end(int fd)
{
    int rc = flock(fd, LOCK_UN);
    if (close(fd) != 0) rc = -1;
    return rc;
}

/* One stat on the publish hot path. The explicit tick has no due gate. */
int fx_action_maintenance_due(const char *root, long long interval)
{
    char path[PATH_MAX];
    struct stat st;
    time_t now = time(NULL);
    if (interval < 0 || now == (time_t)-1 ||
        path_at(root, "action-maintenance.cursor", path, sizeof(path)) != 0)
        return -1;
    if (lstat(path, &st) != 0) return errno == ENOENT ? 1 : -1;
    if (!S_ISREG(st.st_mode)) return -1;
    return st.st_mtime > now || (long long)(now - st.st_mtime) < interval ? 0 : 1;
}

int fx_action_maintenance_load(const char *root, int *shard,
                               long long *offset, long long *last_scan,
                               long long *last_gc)
{
    char path[PATH_MAX], line[128];
    int end = 0;
    if (path_at(root, "action-maintenance.cursor", path, sizeof(path)) != 0)
        return -1;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) {
        if (errno != ENOENT) return -1;
        *shard = 0; *offset = 0; *last_scan = 0; *last_gc = 0;
        return 0;
    }
    ssize_t n = read(fd, line, sizeof(line) - 1);
    int saved = errno;
    close(fd);
    if (n <= 0 || n >= (ssize_t)sizeof(line) - 1) { errno = saved; return -1; }
    line[n] = '\0';
    if (sscanf(line, "FXMAINT1 %d %lld %lld %lld%n", shard, offset,
               last_scan, last_gc, &end) != 4 || end <= 0 ||
        line[end] != '\n' || line[end + 1] != '\0' ||
        *shard < 0 || *shard > 255 || *offset < 0 ||
        *last_scan < 0 || *last_gc < 0) return -1;
    return 0;
}

int fx_action_maintenance_save(const char *root, int shard,
                               long long offset, long long last_scan,
                               long long last_gc)
{
    char meta[PATH_MAX], tmp[PATH_MAX], line[128];
    static unsigned long sequence;
    if (shard < 0 || shard > 255 || offset < 0 || last_scan < 0 ||
        last_gc < 0 ||
        path_at(root, "", meta, sizeof(meta)) != 0) return -1;
    int dfd = open(meta, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (dfd < 0) return -1;
    int fd = -1, n;
    for (int attempt = 0; attempt < 128; ++attempt) {
        n = snprintf(tmp, sizeof(tmp), ".action-maintenance.%ld.%lu",
                     (long)getpid(), __sync_add_and_fetch(&sequence, 1));
        if (n < 0 || (size_t)n >= sizeof(tmp)) break;
        fd = openat(dfd, tmp, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC |
                    O_NOFOLLOW, 0600);
        if (fd >= 0 || errno != EEXIST) break;
    }
    if (fd < 0) { close(dfd); return -1; }
    n = snprintf(line, sizeof(line), "FXMAINT1 %d %lld %lld %lld\n",
                 shard, offset, last_scan, last_gc);
    int rc = n > 0 && n < (int)sizeof(line) ? 0 : -1;
    int written = 0;
    while (rc == 0 && written < n) {
        ssize_t chunk = write(fd, line + written, (size_t)(n - written));
        if (chunk < 0 && errno == EINTR) continue;
        if (chunk <= 0) { rc = -1; break; }
        written += (int)chunk;
    }
    if (rc == 0 && fsync(fd) != 0) rc = -1;
    if (close(fd) != 0) rc = -1;
    if (rc == 0 && renameat(dfd, tmp, dfd, "action-maintenance.cursor") != 0)
        rc = -1;
    if (rc == 0 && fsync(dfd) != 0) rc = -1;
    if (rc != 0) (void)unlinkat(dfd, tmp, 0);
    close(dfd);
    return rc;
}

/* A cursor is a shard plus telldir position. Directory changes may cause a
 * skipped name in one pass; every completed round begins again at shard zero.
 * The caller checkpoints only after processing the returned key. */
int fx_action_maintenance_next(const char *root, int *shard,
                               long long *offset, int *budget, char key[65])
{
    char path[PATH_MAX];
    while (*budget > 0) {
        int n = snprintf(path, sizeof(path), "%s/actions/sha256/%02x",
                         root, *shard);
        if (n < 0 || (size_t)n >= sizeof(path)) return -1;
        DIR *dir = opendir(path);
        if (!dir && errno != ENOENT) return -1;
        if (dir) {
            if (*offset > LONG_MAX) { closedir(dir); return -1; }
            if (*offset != 0) seekdir(dir, (long)*offset);
            for (;;) {
                errno = 0;
                struct dirent *entry = readdir(dir);
                if (!entry) {
                    if (errno) { closedir(dir); return -1; }
                    break;
                }
                --*budget;
                long next = telldir(dir);
                if (next < 0) { closedir(dir); return -1; }
                *offset = next;
                if (valid_key(entry->d_name) &&
                    strncmp(entry->d_name, path + n - 2, 2) == 0) {
                    memcpy(key, entry->d_name, 65);
                    closedir(dir);
                    return 1;
                }
                if (*budget == 0) { closedir(dir); return 0; }
            }
            if (closedir(dir) != 0) return -1;
        }
        *offset = 0;
        *shard = (*shard + 1) & 255;
        --*budget;
        if (*shard == 0) return 0;
    }
    return 0;
}

long long fx_action_maintenance_now(void)
{
    time_t now = time(NULL);
    return now == (time_t)-1 ? -1 : (long long)now;
}
