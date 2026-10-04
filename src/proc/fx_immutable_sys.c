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
    int fd, next;
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
        if (mkdirat(fd, component, 0777) != 0 && errno != EEXIST) {
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
        if (fsync(next) != 0) { close(next); goto fail; }
        /* Also sync when another creator's mkdir is observed: it may still
           be waiting to persist this parent entry. */
        if (fsync(fd) != 0) { close(next); goto fail; }
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

/* The caller owns this descriptor until publication/cleanup; never reopen it. */
int fx_immutable_tempfile(int directory, const char *name)
{
    return openat(directory, name, O_RDWR | O_CREAT | O_EXCL | O_NOFOLLOW, 0600);
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
