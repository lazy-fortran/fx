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

static int sync_dir(const char *path)
{
    int fd = open(path, O_RDONLY);
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

int fx_immutable_mkdirs_sync(const char *path)
{
    char clean[PATH_MAX], part[PATH_MAX], parent[PATH_MAX];
    size_t i, n, start;
    struct stat st;
    if (path == NULL || strlen(path) == 0 || strlen(path) >= sizeof(clean))
        return -1;
    strcpy(clean, path);
    n = strlen(clean);
    while (n > 1 && clean[n - 1] == '/') clean[--n] = '\0';
    start = clean[0] == '/' ? 1 : 0;
    for (i = start; i <= n; ++i) {
        if (clean[i] != '/' && clean[i] != '\0') continue;
        if (i == 0) continue;
        memcpy(part, clean, i);
        part[i] = '\0';
        if (stat(part, &st) == 0) {
            if (!S_ISDIR(st.st_mode)) return -1;
            continue;
        }
        if (errno != ENOENT) return -1;
        if (mkdir(part, 0777) != 0) {
            if (errno != EEXIST || stat(part, &st) != 0 || !S_ISDIR(st.st_mode))
                return -1;
            continue;
        }
        if (parent_path(part, parent, sizeof(parent)) != 0 ||
            sync_dir(parent) != 0 || sync_dir(part) != 0) return -1;
    }
    return 0;
}

int fx_immutable_mkdir_mode(const char *path, int mode)
{
    if (mkdir(path, (mode_t)(mode & 0777)) == 0) return 0;
    return -1;
}

int fx_immutable_chmod_sync(const char *path, int mode)
{
    int fd, rc;
    if (chmod(path, (mode_t)(mode & 0777)) != 0) return -1;
    fd = open(path, O_RDONLY
#ifdef O_DIRECTORY
              | O_DIRECTORY
#endif
    );
    if (fd < 0) return -1;
    rc = fsync(fd);
    close(fd);
    return rc;
}

int fx_immutable_fsync_dir(const char *path)
{
    int fd, rc;
    fd = open(path, O_RDONLY
#ifdef O_DIRECTORY
              | O_DIRECTORY
#endif
    );
    if (fd < 0) return -1;
    rc = fsync(fd);
    close(fd);
    return rc;
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
    int in = -1, out = -1, rc = -1;
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
    char parent[PATH_MAX];
    struct stat st;
    if (lstat(tmp, &st) != 0 || !S_ISREG(st.st_mode)) return -1;
    if (link(tmp, dst) != 0) {
        if (errno == EEXIST) return 1;
        return -1;
    }
    if (unlink(tmp) != 0) return -1;
    if (parent_path(dst, parent, sizeof(parent)) != 0 || sync_dir(parent) != 0)
        return -1;
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
    if (chmod(tmp, (mode_t)(mode & 0777)) != 0 ||
        fx_immutable_fsync_file(tmp) != 0) goto done;
    rc = publish_file(tmp, dst);
    if (rc == 0 && used_clone != NULL) *used_clone = cloned;
done:
    unlink(tmp);
    return rc;
}

int fx_immutable_file_info(const char *path, long long *size_bytes,
                           long long *mtime_ns, long long *inode)
{
    struct stat st;
    if (lstat(path, &st) != 0 || !S_ISREG(st.st_mode) ||
        (st.st_mode & 0222) != 0) return -1;
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
    char parent[PATH_MAX];
    struct stat st;
    int rc;
    if (lstat(tmp, &st) != 0 || !S_ISDIR(st.st_mode)) return -1;
#if defined(__linux__) && defined(SYS_renameat2)
    rc = (int)syscall(SYS_renameat2, AT_FDCWD, tmp, AT_FDCWD, dst,
                      RENAME_NOREPLACE);
#elif defined(__APPLE__)
    rc = renameatx_np(AT_FDCWD, tmp, AT_FDCWD, dst, RENAME_EXCL);
#else
    (void)parent;
    errno = ENOTSUP;
    return 2;
#endif
    if (rc != 0) return errno == EEXIST ? 1 : -1;
    if (parent_path(dst, parent, sizeof(parent)) != 0 || sync_dir(parent) != 0)
        return -1;
    return 0;
}
