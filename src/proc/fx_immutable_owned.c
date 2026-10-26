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
#ifdef _WIN32
#include "../../include/fx_win_store.h"
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif
int fx_immutable_open_directory(const char *path);
int fx_immutable_mkdirs_sync(const char *path);
int fx_immutable_mkdirs_ephemeral(const char *path);
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
/* Refresh an existing object's mtime: a republished or deduplicated object
 * restarts the collector's grace period. 0 ok, -2 missing, -1 error. */
int fx_owned_freshen(int root, int kind, const char *id)
{
    int fd = fx_owned_open_object(root, kind, id);
    if (fd == -2) return -2;
    if (fd < 0) return -1;
#ifdef _WIN32
    int rc = 0;
#else
    int rc = futimens(fd, NULL);
#endif
    close(fd);
    return rc == 0 ? 0 : -1;
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
static void *owned_begin_path(const char *path, int tree, int sync)
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
                                  sizeof(resolved_parent)) != 0) return NULL;
    if (sync) {
        if (fx_immutable_mkdirs_sync(resolved_parent) != 0) return NULL;
    } else if (fx_immutable_mkdirs_ephemeral(resolved_parent) != 0) return NULL;
    fd = fx_immutable_open_directory(resolved_parent);
    if (fd < 0) return NULL;
    t = fx_owned_begin_at(fd, name, tree);
    close(fd);
    return t;
}
void *fx_owned_begin_path(const char *path, int tree)
{
    return owned_begin_path(path, tree, 1);
}
void *fx_owned_begin_path_ephemeral(const char *path, int tree)
{
    return owned_begin_path(path, tree, 0);
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
/* Filesystems such as AFS refuse a hard link between directories (EXDEV or
 * EPERM). Publish a private copy through a same-directory link instead, which
 * keeps the exclusive, atomic name creation. */
static int link_unsupported(int error)
{
    return error == EXDEV || error == EPERM || error == EMLINK ||
        error == ENOSYS || error == EINVAL || error == ENOTSUP ||
        error == EOPNOTSUPP;
}
#if defined(__linux__) && defined(SYS_renameat2)
int fx_rename_dir_noreplace(int from_dir, const char *from, int to_dir,
                            const char *to);
#endif
static int publish_by_copy(owned_t *t)
{
    struct stat st;
    char temp[96];
    int copy = -1, rc, saved;
    if (fstat(t->fd, &st) != 0) return -1;
    for (int i = 0; i < 128 && copy < 0; ++i) {
        unsigned long number = atomic_fetch_add(&serial, 1);
        snprintf(temp, sizeof(temp), ".fx-copy-%ld-%lu", (long)getpid(), number);
        copy = openat(t->parent, temp, O_RDWR | O_CREAT | O_EXCL | O_NOFOLLOW, 0600);
        if (copy < 0 && errno != EEXIST) return -1;
    }
    if (copy < 0) return -1;
    rc = copy_fd(t->fd, copy);
    if (rc == 0) rc = fchmod(copy, st.st_mode & 07777);
    if (rc == 0) rc = fsync(copy);
    if (rc == 0) rc = linkat(t->parent, temp, t->parent, t->name, 0);
    saved = errno;
    (void)unlinkat(t->parent, temp, 0);
    if (rc != 0) { close(copy); errno = saved; return -1; }
    /* Later identity checks bind the published inode, not the staged one. */
    close(t->fd);
    t->fd = copy;
    return 0;
}
static int link_or_copy(owned_t *t)
{
    return link_unsupported(errno) ? publish_by_copy(t) : -1;
}
static int publish_owned_file(owned_t *t, int cas)
{
#ifdef __linux__
    (void)cas;
    /* /proc's fd link binds the held inode even for an unprivileged caller. */
    char source[64];
    snprintf(source, sizeof(source), "/proc/self/fd/%d", t->fd);
    if (linkat(AT_FDCWD, source, t->parent, t->name,
               AT_SYMLINK_FOLLOW) == 0) return 0;
    return link_or_copy(t);
#elif defined(__APPLE__)
    if (cas) {
        /* Atomic exclusive clone from the verified descriptor, never its name. */
        if (fclonefileat(t->fd, t->parent, t->name,
                         CLONE_NOOWNERCOPY) == 0) return 0;
    } else if (linkat(t->staging, "payload", t->parent, t->name, 0) == 0) {
        return 0;
    }
    return link_or_copy(t);
#elif defined(_WIN32)
    (void)cas;
    return fx_win_link_fd(t->fd, t->parent, t->name);
#else
    (void)cas;
    errno = ENOTSUP;
    return -1;
#endif
}
static int retain_publication(owned_t *t, int cas)
{
#ifndef __APPLE__
    (void)cas;
#endif
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
static int publish_boundary(owned_t *t, int cas, int sync)
{
    int rc;
    /* Exclusive link/rename is the only publication boundary: an existing
     * name is retained (rc 1). No directory lock, since flock is host-local
     * on shared filesystems such as BeeGFS. */
    if (t->tree) {
#if defined(__linux__) && defined(SYS_renameat2)
        rc = fx_rename_dir_noreplace(t->parent, t->temp, t->parent, t->name);
#elif defined(__APPLE__)
        rc = renameatx_np(t->parent, t->temp, t->parent, t->name, RENAME_EXCL);
#elif defined(_WIN32)
        rc = fx_win_rename_exclusive(t->parent, t->temp, t->parent, t->name);
#else
        return 2;
#endif
    } else {
        rc = publish_owned_file(t, cas);
    }
    if (rc != 0) {
        if (cas && errno == EEXIST && (!sync || fsync(t->parent) == 0)) {
            rc = 1;
            goto done;
        }
        rc = (errno == ENOTSUP || errno == EOPNOTSUPP) ? 2 : -1;
        goto done;
    }
    if (retain_publication(t, cas) != 0 ||
        (sync && fsync(t->published_fd) != 0)) {
        rc = -1;
        goto done;
    }
    rc = sync ? fsync(t->parent) : 0;
done:
    return rc;
}
static int finish_owned(owned_t *t, int mode, int cas, int sync)
{
    int rc;
    if (!fx_owned_same_entry(t->parent, t->temp, t->staging)) return -1;
    if (!t->tree && !fx_owned_same_entry(t->staging, "payload", t->fd)) return -1;
    if (cas) {
        /* Repeat ownership checks after the synchronized publication boundary. */
        if (!fx_owned_same_entry(t->parent, t->temp, t->staging) ||
            !fx_owned_same_entry(t->staging, "payload", t->fd)) return -1;
    }
    if (fchmod(t->fd, (mode_t)mode) != 0 || (sync && fsync(t->fd) != 0))
        return -1;
    rc = publish_boundary(t, cas, sync);
    return rc;
}
int fx_owned_finish(void *handle, int mode)
{
    return finish_owned(handle, mode, 0, 1);
}
int fx_owned_finish_ephemeral(void *handle, int mode)
{
    return finish_owned(handle, mode, 0, 0);
}
int fx_owned_publish(void *handle) { return finish_owned(handle, 0444, 1, 1); }
int fx_owned_reject(void *handle)
{
    owned_t *t = handle;
    int rc = 0;
    if (!t->published) return 0;
    /* Without a cross-host lock a concurrent collector and publisher may
     * replace the name between check and unlink; that loses a valid object
     * (a later miss), never exposes wrong bytes, as readers verify hashes. */
    if (fx_owned_same_entry(t->parent, t->name, t->published_fd)) {
        if (unlinkat(t->parent, t->name, 0) != 0 && errno != ENOENT) rc = -1;
        else { t->published = 0; rc = fsync(t->parent); }
    }
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
    /* Close the payload first: NFS clients silly-rename an unlinked file
     * that is still open (.nfsXXXX), which would keep the staging
     * directory non-empty and leak it until stale-temp cleanup. */
    if (t->published_fd >= 0) close(t->published_fd);
    if (t->fd >= 0 && t->fd != t->staging) close(t->fd);
    if (!t->tree || !t->published) {
        int removed = remove_contents_fd(t->staging);
        if (removed == 0 && fx_owned_same_entry(t->parent, t->temp, t->staging))
            (void)unlinkat(t->parent, t->temp, AT_REMOVEDIR);
    }
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

/* Batched blob capture with one durability barrier per batch.
 *
 * Invariant: a blob name in the store never refers to bytes that are not yet
 * durable, and a batch commit returns only after every published name is
 * durable. Callers publish the trees or roots that reference the batch only
 * after a successful commit, so a crash can lose staged payloads (removed by
 * the stale-temp collector) but never leaves a published reference to an
 * unsynced blob.
 *
 * Deferred mode (Linux): payloads are written into owned staging directories
 * without fsync. Commit runs syncfs on the store (payloads durable), links
 * every payload under its object name without fsync, then runs syncfs again
 * (names durable): two barriers per batch, independent of the blob count.
 * Eager mode (other platforms, no syncfs): each payload is published at once
 * through the synchronized single-object path, as before batching. */
typedef struct {
    char id[65], temp[96];
    dev_t dev;
    ino_t ino;
} staged_t;
typedef struct {
    int root, deferred, input_valid;
    struct stat input;
    staged_t *items;
    size_t count, capacity;
} batch_t;

#if defined(__linux__) && !defined(FX_BATCH_EAGER)
static int batch_barrier(batch_t *b) { return syncfs(b->root); }
#define BATCH_DEFERRED 1
#else
static int batch_barrier(batch_t *b) { (void)b; return 0; }
#define BATCH_DEFERRED 0
#endif

void *fx_batch_begin(const char *store_root)
{
    batch_t *b = calloc(1, sizeof(*b));
    if (!b) return NULL;
    b->root = fx_immutable_open_directory(store_root);
    if (b->root < 0) { free(b); return NULL; }
    b->deferred = BATCH_DEFERRED;
    return b;
}
int fx_batch_count(void *handle) { return (int)((batch_t *)handle)->count; }
int fx_batch_deferred(void *handle) { return ((batch_t *)handle)->deferred; }
int fx_batch_open_file(const char *path)
{
    return open(path, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
}
/* Start one staged payload for an input descriptor. The input identity is
 * recorded so that stage_end can reject an input changed during the copy. */
void *fx_batch_stage(void *handle, const char *blob_path, int input)
{
    batch_t *b = handle;
    b->input_valid = fstat(input, &b->input) == 0 && S_ISREG(b->input.st_mode);
    if (!b->input_valid) return NULL;
    return owned_begin_path(blob_path, 0, !b->deferred);
}
int fx_batch_write(void *capture, const char *bytes, int count)
{
    owned_t *t = capture;
    int offset = 0;
    while (offset < count) {
        ssize_t n = write(t->fd, bytes + offset, (size_t)(count - offset));
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return -1;
        offset += (int)n;
    }
    return 0;
}
static int input_unchanged(const struct stat *before, int input)
{
    struct stat after;
    return fstat(input, &after) == 0 && before->st_dev == after.st_dev &&
        before->st_ino == after.st_ino && before->st_size == after.st_size &&
        before->st_mtime == after.st_mtime && before->st_ctime == after.st_ctime;
}
/* Finish one payload: 0 staged (deferred) or published (eager), 1 an
 * existing object was retained (caller verifies it), -3 the input changed,
 * -1 failure. The capture handle is always released. */
int fx_batch_stage_end(void *handle, void *capture, int input, const char *id,
                       int ok)
{
    batch_t *b = handle;
    owned_t *t = capture;
    struct stat st;
    int rc = -1;
    if (!b->input_valid || !input_unchanged(&b->input, input)) {
        fx_owned_dispose(t);
        return -3;
    }
    if (!ok || strlen(id) != 64) { fx_owned_dispose(t); return -1; }
    if (!b->deferred) {
        rc = fx_owned_publish(t);
        if (rc != 0 && rc != 1) { (void)fx_owned_reject(t); rc = -1; }
        fx_owned_dispose(t);
        return rc;
    }
    if (b->count == b->capacity) {
        size_t capacity = b->capacity ? 2 * b->capacity : 256;
        staged_t *grown = realloc(b->items, capacity * sizeof(*grown));
        if (!grown) { fx_owned_dispose(t); return -1; }
        b->items = grown;
        b->capacity = capacity;
    }
    if (fchmod(t->fd, 0444) != 0 || fstat(t->fd, &st) != 0 ||
        !fx_owned_same_entry(t->parent, t->temp, t->staging) ||
        !fx_owned_same_entry(t->staging, "payload", t->fd)) {
        fx_owned_dispose(t);
        return -1;
    }
    staged_t *item = &b->items[b->count++];
    memcpy(item->id, id, 64);
    item->id[64] = '\0';
    strcpy(item->temp, t->temp);
    item->dev = st.st_dev;
    item->ino = st.st_ino;
    /* Keep the staging directory for commit; release only descriptors. */
    close(t->fd);
    close(t->staging);
    close(t->parent);
    free(t);
    return 0;
}
static int open_shard(int root, const char *id)
{
    char prefix[3] = {id[0], id[1], '\0'};
    int first = openat(root, "blobs", dir_flags()), second = -1, shard = -1;
    if (first >= 0) second = openat(first, "sha256", dir_flags());
    if (second >= 0) shard = openat(second, prefix, dir_flags());
    if (first >= 0) close(first);
    if (second >= 0) close(second);
    return shard;
}
/* Reopen one staged payload and bind it to its recorded inode. */
static owned_t *reopen_staged(batch_t *b, const staged_t *item)
{
    struct stat st;
    owned_t *t = calloc(1, sizeof(*t));
    if (!t) return NULL;
    t->fd = t->staging = t->published_fd = -1;
    strcpy(t->temp, item->temp);
    strcpy(t->name, item->id);
    t->parent = open_shard(b->root, item->id);
    if (t->parent >= 0) t->staging = openat(t->parent, t->temp, dir_flags());
    if (t->staging >= 0)
        t->fd = openat(t->staging, "payload", O_RDONLY | O_NOFOLLOW);
    if (t->fd < 0 || fstat(t->fd, &st) != 0 || st.st_dev != item->dev ||
        st.st_ino != item->ino || !S_ISREG(st.st_mode) || (st.st_mode & 0222) ||
        !fx_owned_same_entry(t->parent, t->temp, t->staging)) {
        if (t->fd >= 0) close(t->fd);
        if (t->staging >= 0) close(t->staging);
        if (t->parent >= 0) close(t->parent);
        free(t);
        return NULL;
    }
    return t;
}
static void discard_staged(batch_t *b, const staged_t *item)
{
    owned_t *t = reopen_staged(b, item);
    if (t) fx_owned_dispose(t);
}
/* Barrier, publish every staged payload, barrier. retained[i] is set when
 * the name already existed; the caller verifies those objects. A failed
 * first barrier publishes nothing. */
int fx_batch_commit(void *handle, int *retained)
{
    batch_t *b = handle;
    size_t i, published = 0;
    int rc = 0;
    if (!b->deferred || b->count == 0) return 0;
    if (batch_barrier(b) != 0) return -1;
    for (i = 0; i < b->count; ++i) {
        owned_t *t = reopen_staged(b, &b->items[i]);
        int status;
        if (!t) { rc = -1; break; }
        status = publish_boundary(t, 1, 0);
        if (status != 0 && status != 1) {
            (void)fx_owned_reject(t);
            fx_owned_dispose(t);
            rc = -1;
            break;
        }
        retained[i] = status;
        fx_owned_dispose(t);
        b->items[i].temp[0] = '\0';
        ++published;
    }
    /* Names published before a failure are made durable too: they refer to
     * synced bytes, and a later capture deduplicates against them. */
    if (published > 0 && batch_barrier(b) != 0) rc = -1;
    return rc;
}
void fx_batch_end(void *handle)
{
    batch_t *b = handle;
    if (!b) return;
    for (size_t i = 0; i < b->count; ++i)
        if (b->items[i].temp[0]) discard_staged(b, &b->items[i]);
    if (b->root >= 0) close(b->root);
    free(b->items);
    free(b);
}
