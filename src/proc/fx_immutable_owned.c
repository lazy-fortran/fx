/* Descriptor-rooted materialization. Paths are resolved only at acquisition. */
#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdatomic.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <unistd.h>
#ifdef __linux__
#include <linux/fs.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#endif
#ifdef __APPLE__
#include <sys/clonefile.h>
#endif
#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
int fx_immutable_open_directory(const char *path);
int fx_immutable_mkdirs_sync(const char *path);
int fx_immutable_resolve_root(const char *path, char *resolved, size_t capacity);
int fx_immutable_tempfile(int directory, const char *name);

typedef struct {
    int parent, fd, staging, tree, published, published_fd;
    char temp[96], name[PATH_MAX];
} owned_t;
static _Atomic unsigned long serial;
static int dir_flags(void) { return O_RDONLY | O_DIRECTORY | O_NOFOLLOW; }

int fx_owned_open_store(const char *path)
{
    return fx_immutable_open_directory(path);
}
int fx_owned_open_object(int root, int kind, const char *id)
{
    int first = -1, second = -1, shard = -1, fd = -1;
    char prefix[3] = {id[0], id[1], '\0'};
    struct stat st;
    first = openat(root, kind == 1 ? "blobs" : "trees", dir_flags());
    if (first >= 0) second = openat(first, "sha256", dir_flags());
    if (second >= 0) shard = openat(second, prefix, dir_flags());
    if (shard >= 0) fd = openat(shard, id, O_RDONLY | O_NOFOLLOW);
    int saved = errno;
    if (first >= 0) close(first);
    if (second >= 0) close(second);
    if (shard >= 0) close(shard);
    if (fd < 0) return saved == ENOENT ? -2 : -3;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode) || (st.st_mode & 0222)) {
        close(fd);
        return -3;
    }
    return fd;
}
int fx_owned_fd_size(int fd, long long *size)
{
    struct stat st;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode)) return -1;
    *size = (long long)st.st_size;
    return 0;
}
long long fx_owned_read(int fd, long long offset, char *bytes, int count)
{
    ssize_t n;
    do { n = pread(fd, bytes, (size_t)count, (off_t)offset); }
    while (n < 0 && errno == EINTR);
    return (long long)n;
}
int fx_owned_same_entry(int parent, const char *name, int fd)
{
    struct stat entry, held;
    return fstat(fd, &held) == 0 &&
        fstatat(parent, name, &entry, AT_SYMLINK_NOFOLLOW) == 0 &&
        held.st_dev == entry.st_dev && held.st_ino == entry.st_ino &&
        (held.st_mode & S_IFMT) == (entry.st_mode & S_IFMT);
}
static int make_owned(owned_t *t)
{
    for (int i = 0; i < 128; ++i) {
        unsigned long number = atomic_fetch_add(&serial, 1);
        snprintf(t->temp, sizeof(t->temp), ".fx-owned-%ld-%lu", (long)getpid(), number);
        {
            if (mkdirat(t->parent, t->temp, 0700) != 0) {
                if (errno == EEXIST) continue;
                return -1;
            }
            t->staging = openat(t->parent, t->temp, dir_flags());
        }
        if (t->staging < 0 ||
            !fx_owned_same_entry(t->parent, t->temp, t->staging)) return -1;
        t->fd = t->tree ? dup(t->staging) : fx_immutable_tempfile(t->staging, "payload");
        return t->fd >= 0 ? 0 : -1;
    }
    return -1;
}
void *fx_owned_begin_at(int parent, const char *name, int tree)
{
    owned_t *t;
    if (!name || !*name || strchr(name, '/') || !strcmp(name, ".") ||
        !strcmp(name, "..") || strlen(name) >= PATH_MAX) return NULL;
    t = calloc(1, sizeof(*t));
    if (!t) return NULL;
    t->parent = dup(parent);
    t->fd = -1;
    t->published_fd = -1;
    t->staging = -1;
    t->tree = tree;
    strcpy(t->name, name);
    if (t->parent < 0 || make_owned(t) != 0) {
        if (t->fd >= 0) close(t->fd);
        if (t->staging >= 0) close(t->staging);
        if (t->parent >= 0) close(t->parent);
        free(t);
        return NULL;
    }
    return t;
}
void *fx_owned_begin_path(const char *path, int tree)
{
    char parent[PATH_MAX], resolved_parent[PATH_MAX];
    const char *slash = strrchr(path, '/');
    const char *name = slash ? slash + 1 : path;
    size_t length = slash ? (size_t)(slash - path) : 1;
    int fd;
    owned_t *t;
    if (length == 0) length = 1;
    if (length >= sizeof(parent)) return NULL;
    if (slash) { memcpy(parent, path, length); parent[length] = '\0'; }
    else strcpy(parent, ".");
    if (fx_immutable_resolve_root(parent, resolved_parent,
                                  sizeof(resolved_parent)) != 0 ||
        fx_immutable_mkdirs_sync(resolved_parent) != 0) return NULL;
    fd = fx_immutable_open_directory(resolved_parent);
    if (fd < 0) return NULL;
    t = fx_owned_begin_at(fd, name, tree);
    close(fd);
    return t;
}
int fx_owned_fd(void *handle) { return ((owned_t *)handle)->fd; }
static int copy_fd(int input, int output)
{
    char bytes[65536];
    ssize_t n, written;
    off_t offset = 0;
    for (;;) {
        do { n = pread(input, bytes, sizeof(bytes), offset); }
        while (n < 0 && errno == EINTR);
        if (n == 0) return 0;
        if (n < 0) return -1;
        ssize_t position = 0;
        while (position < n) {
            do { written = write(output, bytes + position, (size_t)(n - position)); }
            while (written < 0 && errno == EINTR);
            if (written <= 0) return -1;
            position += written;
        }
        offset += n;
    }
}
static int clone_fd(int input, owned_t *t)
{
#ifdef __linux__
    return ioctl(t->fd, FICLONE, input);
#elif defined(__APPLE__)
    if (!fx_owned_same_entry(t->staging, "payload", t->fd)) return -1;
    if (unlinkat(t->staging, "payload", 0) != 0) return -1;
    close(t->fd);
    t->fd = -1;
    if (fclonefileat(input, t->staging, "payload", 0) != 0) {
        t->fd = openat(t->staging, "payload",
            O_RDWR | O_CREAT | O_EXCL | O_NOFOLLOW, 0600);
        return -1;
    }
    t->fd = openat(t->staging, "payload", O_RDONLY | O_NOFOLLOW);
    return t->fd >= 0 ? 0 : -1;
#else
    (void)input; (void)t;
    return -1;
#endif
}
int fx_owned_fill(void *handle, int input, int strategy, int *cloned)
{
    owned_t *t = handle;
    *cloned = 0;
    if (strategy != 1 && clone_fd(input, t) == 0) *cloned = 1;
    if (strategy == 2 && !*cloned) return 2;
    if (t->fd < 0) return -1;
    if (!*cloned) {
        if (ftruncate(t->fd, 0) != 0 || lseek(t->fd, 0, SEEK_SET) < 0) return -1;
        if (copy_fd(input, t->fd) != 0) return -1;
    }
    return 0;
}
static int publish_owned_file(owned_t *t, int cas)
{
#ifdef __linux__
    (void)cas;
    /* /proc's fd link binds the held inode even for an unprivileged caller. */
    char source[64];
    snprintf(source, sizeof(source), "/proc/self/fd/%d", t->fd);
    return linkat(AT_FDCWD, source, t->parent, t->name, AT_SYMLINK_FOLLOW);
#elif defined(__APPLE__)
    if (cas) {
        /* Atomic exclusive clone from the verified descriptor, never its name. */
        return fclonefileat(t->fd, t->parent, t->name, CLONE_NOOWNERCOPY);
    }
    return linkat(t->staging, "payload", t->parent, t->name, 0);
#else
    (void)cas;
    errno = ENOTSUP;
    return -1;
#endif
}
static int retain_publication(owned_t *t, int cas)
{
    t->published = 1;
#ifdef __APPLE__
    if (cas) t->published_fd = openat(t->parent, t->name, O_RDONLY | O_NOFOLLOW);
    else
#endif
    t->published_fd = dup(t->fd);
    if (t->published_fd < 0) return -1;
    if (!fx_owned_same_entry(t->parent, t->name, t->published_fd)) return -1;
    return 0;
}
static int lock_parent(owned_t *t)
{
    int rc;
    do { rc = flock(t->parent, LOCK_EX); } while (rc != 0 && errno == EINTR);
    return rc;
}
static int publish_boundary(owned_t *t, int cas)
{
    int rc;
    if (cas && lock_parent(t) != 0) return -1;
    if (t->tree) {
#if defined(__linux__) && defined(SYS_renameat2)
        rc = (int)syscall(SYS_renameat2, t->parent, t->temp,
            t->parent, t->name, RENAME_NOREPLACE);
#elif defined(__APPLE__)
        rc = renameatx_np(t->parent, t->temp, t->parent, t->name, RENAME_EXCL);
#else
        return 2;
#endif
    } else {
        rc = publish_owned_file(t, cas);
    }
    if (rc != 0) {
        if (cas && errno == EEXIST && fsync(t->parent) == 0) {
            rc = 1;
            goto done;
        }
        rc = (errno == ENOTSUP || errno == EOPNOTSUPP) ? 2 : -1;
        goto done;
    }
    if (retain_publication(t, cas) != 0 || fsync(t->published_fd) != 0) {
        rc = -1;
        goto done;
    }
    rc = fsync(t->parent);
done:
    if (cas && flock(t->parent, LOCK_UN) != 0) return -1;
    return rc;
}
static int finish_owned(owned_t *t, int mode, int cas)
{
    int rc;
    if (!fx_owned_same_entry(t->parent, t->temp, t->staging)) return -1;
    if (!t->tree && !fx_owned_same_entry(t->staging, "payload", t->fd)) return -1;
    if (cas) {
        /* Repeat ownership checks after the synchronized publication boundary. */
        if (!fx_owned_same_entry(t->parent, t->temp, t->staging) ||
            !fx_owned_same_entry(t->staging, "payload", t->fd)) return -1;
    }
    if (fchmod(t->fd, (mode_t)mode) != 0 || fsync(t->fd) != 0) return -1;
    rc = publish_boundary(t, cas);
    return rc;
}
int fx_owned_finish(void *handle, int mode) { return finish_owned(handle, mode, 0); }
int fx_owned_publish(void *handle) { return finish_owned(handle, 0444, 1); }
int fx_owned_reject(void *handle)
{
    owned_t *t = handle;
    int rc = 0;
    if (!t->published) return 0;
    if (lock_parent(t) != 0) return -1;
    /* The held-directory lock excludes valid publishers across check/unlink. */
    if (fx_owned_same_entry(t->parent, t->name, t->published_fd)) {
        if (unlinkat(t->parent, t->name, 0) != 0) rc = -1;
        else { t->published = 0; rc = fsync(t->parent); }
    }
    if (flock(t->parent, LOCK_UN) != 0) rc = -1;
    return rc;
}
int fx_owned_write(void *handle, const char *text, int count)
{
    owned_t *t = handle;
    int offset = 0;
    while (offset < count) {
        ssize_t n = write(t->fd, text + offset, (size_t)(count - offset));
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return -1;
        offset += (int)n;
    }
    return fsync(t->fd);
}
int fx_owned_copy_source(void *handle, const char *path)
{
    owned_t *t = handle;
    char bytes[65536];
    struct stat st;
    int input = open(path, O_RDONLY), rc = -1;
    if (input < 0 || fstat(input, &st) != 0 || !S_ISREG(st.st_mode)) goto done;
    for (;;) {
        ssize_t n = read(input, bytes, sizeof(bytes)), offset = 0;
        if (n < 0 && errno == EINTR) continue;
        if (n < 0) goto done;
        if (!n) break;
        while (offset < n) {
            ssize_t written = write(t->fd, bytes + offset, (size_t)(n - offset));
            if (written < 0 && errno == EINTR) continue;
            if (written <= 0) goto done;
            offset += written;
        }
    }
    rc = fsync(t->fd);
done:
    if (input >= 0 && close(input) != 0) rc = -1;
    return rc;
}
static int remove_contents_fd(int fd)
{
    DIR *directory = fdopendir(dup(fd));
    struct dirent *entry;
    struct stat st;
    int rc = 0;
    if (!directory) return -1;
    rewinddir(directory);
    while ((entry = readdir(directory)) != NULL) {
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        if (fstatat(fd, entry->d_name, &st, AT_SYMLINK_NOFOLLOW) != 0) { rc = -1; break; }
        if (S_ISDIR(st.st_mode)) {
            int child = openat(fd, entry->d_name, dir_flags());
            if (child < 0) { rc = -1; break; }
            int removed = remove_contents_fd(child);
            if (removed == 0 && fx_owned_same_entry(fd, entry->d_name, child))
                removed = unlinkat(fd, entry->d_name, AT_REMOVEDIR);
            close(child);
            if (removed != 0) { rc = -1; break; }
        } else if (unlinkat(fd, entry->d_name, 0) != 0) { rc = -1; break; }
    }
    closedir(directory);
    return rc;
}
void fx_owned_dispose(void *handle)
{
    owned_t *t = handle;
    if (!t) return;
    if (!t->tree || !t->published) {
        int removed = remove_contents_fd(t->staging);
        if (removed == 0 && fx_owned_same_entry(t->parent, t->temp, t->staging))
            (void)unlinkat(t->parent, t->temp, AT_REMOVEDIR);
    }
    if (t->published_fd >= 0) close(t->published_fd);
    close(t->fd);
    close(t->staging);
    close(t->parent);
    free(t);
}
int fx_owned_sync(int fd) { return fsync(fd); }
int fx_owned_close(int fd) { return close(fd); }
int fx_owned_file_info(int fd, long long *size, long long *mtime, long long *inode)
{
    struct stat st;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode) || (st.st_mode & 0222)) return -1;
    *size = (long long)st.st_size;
#ifdef __APPLE__
    *mtime = (long long)st.st_mtimespec.tv_sec * 1000000000LL + st.st_mtimespec.tv_nsec;
#else
    *mtime = (long long)st.st_mtim.tv_sec * 1000000000LL + st.st_mtim.tv_nsec;
#endif
    *inode = (long long)st.st_ino;
    return 0;
}
