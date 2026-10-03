/* Filesystem primitives for fx's immutable blob/tree store. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>
#include <time.h>
#ifdef __linux__
#include <linux/fs.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#endif
#ifdef __APPLE__
#include <stdio.h>
#include <sys/attr.h>
#include <sys/clonefile.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static char test_copy_ready[PATH_MAX];
static char test_copy_release[PATH_MAX];
static char test_publish_ready[PATH_MAX];
static char test_publish_release[PATH_MAX];
static char test_eexist_marker[PATH_MAX];
static char test_mkdir_pause_path[PATH_MAX];
static char test_mkdir_pause_ready[PATH_MAX];
static char test_mkdir_pause_release[PATH_MAX];
static char test_mkdir_existing_marker[PATH_MAX];

static void copy_path(char *dst, size_t cap, const char *src)
{
    size_t n = src == NULL ? 0 : strlen(src);
    if (n >= cap) n = 0;
    if (n > 0) memcpy(dst, src, n);
    dst[n] = '\0';
}

static int directory_flags(void)
{
    int flags = O_RDONLY;
#ifdef O_DIRECTORY
    flags |= O_DIRECTORY;
#endif
#ifdef O_NOFOLLOW
    flags |= O_NOFOLLOW;
#endif
    return flags;
}

static int open_existing_directory(const char *path)
{
    char clean[PATH_MAX], component[PATH_MAX];
    const char *cursor, *start;
    size_t n, length;
    int fd, next;
    struct stat st;
    if (path == NULL || (n = strlen(path)) == 0 || n >= sizeof(clean)) {
        errno = EINVAL;
        return -1;
    }
    memcpy(clean, path, n + 1);
    fd = open(clean[0] == '/' ? "/" : ".", directory_flags());
    if (fd < 0) return -1;
    cursor = clean;
    while (*cursor == '/') ++cursor;
    while (*cursor != '\0') {
        start = cursor;
        while (*cursor != '\0' && *cursor != '/') ++cursor;
        length = (size_t)(cursor - start);
        while (*cursor == '/') ++cursor;
        if (length == 1 && start[0] == '.') continue;
        if (length == 0 || length >= sizeof(component)) {
            close(fd);
            errno = ENAMETOOLONG;
            return -1;
        }
        memcpy(component, start, length);
        component[length] = '\0';
        next = openat(fd, component, directory_flags());
        if (next < 0) {
            close(fd);
            return -1;
        }
        if (fstat(next, &st) != 0 || !S_ISDIR(st.st_mode)) {
            close(next);
            close(fd);
            errno = ENOTDIR;
            return -1;
        }
        close(fd);
        fd = next;
    }
    return fd;
}

int fx_immutable_open_directory(const char *path)
{
    return open_existing_directory(path);
}

static void record_test_marker(const char *path, const char *text)
{
    int fd;
    size_t length, written = 0;
    ssize_t n;
    if (path == NULL || path[0] == '\0') return;
    fd = open(path, O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (fd < 0) return;
    length = strlen(text);
    while (written < length) {
        n = write(fd, text + written, length - written);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) break;
        written += (size_t)n;
    }
    if (written == length) (void)write(fd, "\n", 1);
    (void)fsync(fd);
    (void)close(fd);
}

void fx_immutable_test_configure(const char *ready, const char *release,
                                 const char *publish_ready,
                                 const char *publish_release,
                                 const char *eexist)
{
    copy_path(test_copy_ready, sizeof(test_copy_ready), ready);
    copy_path(test_copy_release, sizeof(test_copy_release), release);
    copy_path(test_publish_ready, sizeof(test_publish_ready), publish_ready);
    copy_path(test_publish_release, sizeof(test_publish_release),
              publish_release);
    copy_path(test_eexist_marker, sizeof(test_eexist_marker), eexist);
}

void fx_immutable_test_mkdir_configure(const char *path, const char *ready,
                                       const char *release,
                                       const char *existing)
{
    copy_path(test_mkdir_pause_path, sizeof(test_mkdir_pause_path), path);
    copy_path(test_mkdir_pause_ready, sizeof(test_mkdir_pause_ready), ready);
    copy_path(test_mkdir_pause_release, sizeof(test_mkdir_pause_release),
              release);
    copy_path(test_mkdir_existing_marker, sizeof(test_mkdir_existing_marker),
              existing);
}

static void pause_after_temp_write(const char *temp_path)
{
    struct timespec delay = {0, 10000000};
    if (test_copy_ready[0] == '\0' || test_copy_release[0] == '\0') return;
    record_test_marker(test_copy_ready, temp_path);
    while (access(test_copy_release, F_OK) != 0) (void)nanosleep(&delay, NULL);
    test_copy_ready[0] = '\0';
}

static void pause_before_publish(const char *temp_path)
{
    struct timespec delay = {0, 10000000};
    if (test_publish_ready[0] == '\0' || test_publish_release[0] == '\0')
        return;
    record_test_marker(test_publish_ready, temp_path);
    while (access(test_publish_release, F_OK) != 0) (void)nanosleep(&delay, NULL);
    test_publish_ready[0] = '\0';
}

static void pause_mkdir_parent_sync(const char *component_path)
{
    struct timespec delay = {0, 10000000};
    if (test_mkdir_pause_path[0] == '\0' ||
        strcmp(test_mkdir_pause_path, component_path) != 0 ||
        test_mkdir_pause_ready[0] == '\0' ||
        test_mkdir_pause_release[0] == '\0') return;
    record_test_marker(test_mkdir_pause_ready, component_path);
    while (access(test_mkdir_pause_release, F_OK) != 0)
        (void)nanosleep(&delay, NULL);
    test_mkdir_pause_path[0] = '\0';
}

static int sync_dir(const char *path)
{
    int fd = open_existing_directory(path);
    int rc;
    if (fd < 0) return -1;
    rc = fsync(fd);
    close(fd);
    return rc;
}

static int parent_path(const char *path, char *parent, size_t cap)
{
    const char *slash = strrchr(path, '/');
    size_t n;
    if (slash == NULL) {
        if (cap < 2) return -1;
        strcpy(parent, ".");
        return 0;
    }
    n = (size_t)(slash - path);
    if (n == 0) n = 1;
    if (n >= cap) return -1;
    memcpy(parent, path, n);
    parent[n] = '\0';
    return 0;
}

static int base_name(const char *path, char *base, size_t cap)
{
    const char *slash = strrchr(path, '/');
    const char *name = slash == NULL ? path : slash + 1;
    size_t n = strlen(name);
    if (n == 0 || n >= cap) return -1;
    memcpy(base, name, n + 1);
    return 0;
}

static int open_matching_parent(const char *src, const char *dst,
                                char *base_src, char *base_dst,
                                size_t base_cap)
{
    char src_parent[PATH_MAX], dst_parent[PATH_MAX];
    int fd;
    if (parent_path(src, src_parent, sizeof(src_parent)) != 0 ||
        parent_path(dst, dst_parent, sizeof(dst_parent)) != 0 ||
        strcmp(src_parent, dst_parent) != 0 ||
        base_name(src, base_src, base_cap) != 0 ||
        base_name(dst, base_dst, base_cap) != 0) {
        errno = EINVAL;
        return -1;
    }
    fd = open_existing_directory(dst_parent);
    return fd;
}

int fx_immutable_mkdirs_sync(const char *path)
{
    char clean[PATH_MAX], component[PATH_MAX], walked[PATH_MAX];
    const char *cursor, *start;
    size_t n, length, used;
    struct stat st;
    int fd, next, created;
    if (path == NULL || (n = strlen(path)) == 0 || n >= sizeof(clean))
        return -1;
    memcpy(clean, path, n + 1);
    while (n > 1 && clean[n - 1] == '/') clean[--n] = '\0';
    fd = open(clean[0] == '/' ? "/" : ".", directory_flags());
    if (fd < 0) return -1;
    if (clean[0] == '/') {
        strcpy(walked, "/");
        used = 1;
    } else {
        strcpy(walked, ".");
        used = 1;
    }
    cursor = clean;
    while (*cursor == '/') ++cursor;
    while (*cursor != '\0') {
        start = cursor;
        while (*cursor != '\0' && *cursor != '/') ++cursor;
        length = (size_t)(cursor - start);
        while (*cursor == '/') ++cursor;
        if (length == 0 || length >= sizeof(component)) goto fail;
        memcpy(component, start, length);
        component[length] = '\0';
        if (length == 1 && component[0] == '.') continue;
        if (mkdirat(fd, component, 0777) == 0) {
            created = 1;
        } else if (errno == EEXIST) {
            created = 0;
        } else {
            goto fail;
        }
        next = openat(fd, component, directory_flags());
        if (next < 0 || fstat(next, &st) != 0 || !S_ISDIR(st.st_mode)) {
            if (next >= 0) close(next);
            goto fail;
        }
        if (walked[used - 1] != '/' &&
            !(used == 1 && walked[0] == '.')) walked[used++] = '/';
        else if (used == 1 && walked[0] == '.') walked[used++] = '/';
        if (used + length >= sizeof(walked)) { close(next); goto fail; }
        memcpy(walked + used, component, length);
        used += length;
        walked[used] = '\0';
        if (created) pause_mkdir_parent_sync(walked);
        if (fsync(next) != 0) { close(next); goto fail; }
        /* Also sync when another creator's mkdir is observed: it may still
           be waiting to persist this parent entry. */
        if (fsync(fd) != 0) { close(next); goto fail; }
        if (!created && test_mkdir_existing_marker[0] != '\0' &&
            strcmp(test_mkdir_pause_path, walked) == 0)
            record_test_marker(test_mkdir_existing_marker,
                               "EXIST_PARENT_SYNC");
        close(fd);
        fd = next;
    }
    close(fd);
    return 0;
fail:
    close(fd);
    return -1;
}

int fx_immutable_mkdir_mode(const char *path, int mode)
{
    if (mkdir(path, (mode_t)(mode & 0777)) == 0) return 0;
    return -1;
}

int fx_immutable_chmod_sync(const char *path, int mode)
{
    int fd, rc;
    struct stat st;
    fd = open(path, O_RDONLY
#ifdef O_NOFOLLOW
              | O_NOFOLLOW
#endif
    );
    if (fd < 0) return -1;
    if (fstat(fd, &st) != 0 ||
        (!S_ISREG(st.st_mode) && !S_ISDIR(st.st_mode))) {
        close(fd);
        errno = EINVAL;
        return -1;
    }
    /* Keep the already-open descriptor across chmod; a clone may begin
       read-only, and mode 000 must not prevent the durability barrier. */
    if (fchmod(fd, (mode_t)(mode & 0777)) != 0) { close(fd); return -1; }
    rc = fsync(fd);
    close(fd);
    return rc;
}

int fx_immutable_fsync_dir(const char *path)
{
    return sync_dir(path);
}

int fx_immutable_tempfile(const char *dir, char *out, int cap)
{
    char tmpl[PATH_MAX];
    int fd;
    int n = snprintf(tmpl, sizeof(tmpl), "%s/.fx-tmp-XXXXXX", dir);
    if (n < 0 || (size_t)n >= sizeof(tmpl)) return -1;
    fd = mkstemp(tmpl);
    if (fd < 0) return -1;
    if (close(fd) != 0) {
        unlink(tmpl);
        return -1;
    }
    n = snprintf(out, (size_t)cap, "%s", tmpl);
    if (n < 0 || n >= cap) {
        unlink(tmpl);
        return -1;
    }
    return 0;
}

int fx_immutable_tempdir(const char *parent, char *out, int cap)
{
    char tmpl[PATH_MAX];
    int n = snprintf(tmpl, sizeof(tmpl), "%s/.fx-tree-XXXXXX", parent);
    if (n < 0 || (size_t)n >= sizeof(tmpl)) return -1;
    if (mkdtemp(tmpl) == NULL) return -1;
    n = snprintf(out, (size_t)cap, "%s", tmpl);
    if (n < 0 || n >= cap) return -1;
    return 0;
}

int fx_immutable_fsync_file(const char *path)
{
    int fd = open(path, O_WRONLY);
    int rc;
    if (fd < 0) return -1;
    rc = fsync(fd);
    close(fd);
    return rc;
}

int fx_immutable_seal_file(const char *path)
{
    int fd = open(path, O_WRONLY);
    int rc;
    if (fd < 0) return -1;
    if (fsync(fd) != 0 || chmod(path, 0444) != 0) {
        close(fd);
        return -1;
    }
    rc = fsync(fd);
    close(fd);
    return rc;
}

int fx_immutable_copy_sync(const char *src, const char *dst)
{
    unsigned char buf[65536];
    struct stat st;
    ssize_t nread, nwritten, off;
    int in = -1, out = -1, rc = -1, paused = 0;
    in = open(src, O_RDONLY);
    if (in < 0 || fstat(in, &st) != 0 || !S_ISREG(st.st_mode)) goto done;
    out = open(dst, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (out < 0) goto done;
    for (;;) {
        nread = read(in, buf, sizeof(buf));
        if (nread == 0) break;
        if (nread < 0) {
            if (errno == EINTR) continue;
            goto done;
        }
        off = 0;
        while (off < nread) {
            nwritten = write(out, buf + off, (size_t)(nread - off));
            if (nwritten < 0 && errno == EINTR) continue;
            if (nwritten <= 0) goto done;
            off += nwritten;
        }
        if (!paused) {
            pause_after_temp_write(dst);
            paused = 1;
        }
    }
    if (fsync(out) != 0) goto done;
    rc = 0;
done:
    if (in >= 0) close(in);
    if (out >= 0 && close(out) != 0) rc = -1;
    return rc;
}

static int publish_file(const char *tmp, const char *dst)
{
    char base_tmp[PATH_MAX], base_dst[PATH_MAX];
    struct stat st;
    int dirfd, rc;
    dirfd = open_matching_parent(tmp, dst, base_tmp, base_dst,
                                 sizeof(base_tmp));
    if (dirfd < 0) return -1;
    if (fstatat(dirfd, base_tmp, &st, AT_SYMLINK_NOFOLLOW) != 0 ||
        !S_ISREG(st.st_mode)) { close(dirfd); return -1; }
    pause_before_publish(tmp);
    if (linkat(dirfd, base_tmp, dirfd, base_dst, 0) != 0) {
        if (errno == EEXIST) {
            rc = fsync(dirfd);
            if (rc == 0) record_test_marker(test_eexist_marker, "EEXIST");
            close(dirfd);
            return rc == 0 ? 1 : -1;
        }
        close(dirfd);
        return -1;
    }
    if (unlinkat(dirfd, base_tmp, 0) != 0 || fsync(dirfd) != 0) {
        close(dirfd);
        return -1;
    }
    close(dirfd);
    return 0;
}

int fx_immutable_publish_file(const char *tmp, const char *dst)
{
    return publish_file(tmp, dst);
}

static int try_clone(const char *src, const char *tmp)
{
#ifdef __linux__
    int in = -1, out = -1, rc = -1;
    in = open(src, O_RDONLY);
    out = open(tmp, O_WRONLY);
    if (in >= 0 && out >= 0 && ioctl(out, FICLONE, in) == 0) rc = 0;
    if (in >= 0) close(in);
    if (out >= 0) close(out);
    return rc;
#elif defined(__APPLE__)
    if (unlink(tmp) != 0) return -1;
    return clonefile(src, tmp, 0);
#else
    (void)src; (void)tmp;
    errno = ENOTSUP;
    return -1;
#endif
}

int fx_immutable_materialize(const char *src, const char *dst, int mode,
                             int strategy, int *used_clone)
{
    char parent[PATH_MAX], tmp[PATH_MAX];
    int status, rc = -1, cloned = 0;
    if (used_clone != NULL) *used_clone = 0;
    if (parent_path(dst, parent, sizeof(parent)) != 0) return -1;
    status = fx_immutable_tempfile(parent, tmp, sizeof(tmp));
    if (status != 0) return -1;
    if (strategy != 1 && try_clone(src, tmp) == 0) cloned = 1;
    if (strategy == 2 && !cloned) {
        unlink(tmp);
        return 2;
    }
    if (!cloned && fx_immutable_copy_sync(src, tmp) != 0) goto done;
    if (fx_immutable_chmod_sync(tmp, mode) != 0) goto done;
    rc = publish_file(tmp, dst);
    if (rc == 0 && used_clone != NULL) *used_clone = cloned;
done:
    unlink(tmp);
    return rc;
}

int fx_immutable_file_info(const char *path, long long *size_bytes,
                           long long *mtime_ns, long long *inode)
{
    char parent[PATH_MAX], base[PATH_MAX];
    struct stat st;
    int dirfd;
    if (path == NULL || parent_path(path, parent, sizeof(parent)) != 0 ||
        base_name(path, base, sizeof(base)) != 0) return -1;
    dirfd = open_existing_directory(parent);
    if (dirfd < 0) return errno == ENOENT ? 1 : -1;
    if (fstatat(dirfd, base, &st, AT_SYMLINK_NOFOLLOW) != 0) {
        int err = errno;
        close(dirfd);
        return err == ENOENT ? 1 : -1;
    }
    close(dirfd);
    if (!S_ISREG(st.st_mode) || (st.st_mode & 0222) != 0) return -1;
    if (size_bytes != NULL) *size_bytes = (long long)st.st_size;
#if defined(__APPLE__)
    if (mtime_ns != NULL) *mtime_ns = (long long)st.st_mtimespec.tv_sec *
        1000000000LL + (long long)st.st_mtimespec.tv_nsec;
#else
    if (mtime_ns != NULL) *mtime_ns = (long long)st.st_mtim.tv_sec *
        1000000000LL + (long long)st.st_mtim.tv_nsec;
#endif
    if (inode != NULL) *inode = (long long)st.st_ino;
    return 0;
}

int fx_immutable_path_mode(const char *path, int *mode)
{
    char parent[PATH_MAX], base[PATH_MAX];
    struct stat st;
    int dirfd;
    if (path == NULL || mode == NULL ||
        parent_path(path, parent, sizeof(parent)) != 0 ||
        base_name(path, base, sizeof(base)) != 0) return -1;
    dirfd = open_existing_directory(parent);
    if (dirfd < 0) return -1;
    if (fstatat(dirfd, base, &st, AT_SYMLINK_NOFOLLOW) != 0) {
        close(dirfd);
        return -1;
    }
    close(dirfd);
    *mode = (int)(st.st_mode & 0777);
    return 0;
}

static int remove_tree_rec(const char *path)
{
    DIR *dir = opendir(path);
    struct dirent *entry;
    char child[PATH_MAX];
    struct stat st;
    if (dir == NULL) return unlink(path);
    while ((entry = readdir(dir)) != NULL) {
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, ".."))
            continue;
        if (snprintf(child, sizeof(child), "%s/%s", path, entry->d_name) >=
            (int)sizeof(child)) { closedir(dir); return -1; }
        if (lstat(child, &st) != 0) { closedir(dir); return -1; }
        if (S_ISDIR(st.st_mode)) {
            if (remove_tree_rec(child) != 0) { closedir(dir); return -1; }
        } else if (unlink(child) != 0) { closedir(dir); return -1; }
    }
    closedir(dir);
    return rmdir(path);
}

int fx_immutable_remove_tree(const char *path)
{
    return remove_tree_rec(path);
}

int fx_immutable_publish_tree(const char *tmp, const char *dst)
{
    char base_tmp[PATH_MAX], base_dst[PATH_MAX];
    struct stat st;
    int dirfd, rc;
    dirfd = open_matching_parent(tmp, dst, base_tmp, base_dst,
                                 sizeof(base_tmp));
    if (dirfd < 0) return -1;
    if (fstatat(dirfd, base_tmp, &st, AT_SYMLINK_NOFOLLOW) != 0 ||
        !S_ISDIR(st.st_mode)) { close(dirfd); return -1; }
    pause_before_publish(tmp);
#if defined(__linux__) && defined(SYS_renameat2)
    rc = (int)syscall(SYS_renameat2, dirfd, base_tmp, dirfd, base_dst,
                      RENAME_NOREPLACE);
#elif defined(__APPLE__)
    rc = renameatx_np(dirfd, base_tmp, dirfd, base_dst, RENAME_EXCL);
#else
    close(dirfd);
    errno = ENOTSUP;
    return 2;
#endif
    if (rc != 0) {
        if (errno == EEXIST) {
            rc = fsync(dirfd);
            if (rc == 0) record_test_marker(test_eexist_marker, "EEXIST");
            close(dirfd);
            return rc == 0 ? 1 : -1;
        }
        close(dirfd);
        return -1;
    }
    rc = fsync(dirfd);
    close(dirfd);
    if (rc != 0) return -1;
    return 0;
}

static int owned_pause_phase;
static char owned_pause_ready[PATH_MAX], owned_pause_release[PATH_MAX];
void fx_immutable_owned_test_configure(int phase, const char *ready,
                                     const char *release)
{
    owned_pause_phase = phase;
    copy_path(owned_pause_ready, sizeof(owned_pause_ready), ready);
    copy_path(owned_pause_release, sizeof(owned_pause_release), release);
}
void fx_immutable_owned_pause(int phase, const char *path)
{
    struct timespec delay = {0, 10000000};
    if (phase != owned_pause_phase || !owned_pause_ready[0] || !path[0]) return;
    record_test_marker(owned_pause_ready, path);
    for (int i = 0; i < 3000; ++i) {
        if (access(owned_pause_release, F_OK) == 0) break;
        (void)nanosleep(&delay, NULL);
    }
    owned_pause_phase = 0;
}
