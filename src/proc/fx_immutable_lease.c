#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
#define SNAPSHOT_LIMIT (16 * 1024 * 1024)
#define ROW_LIMIT 65536

static int safe_field(const char *s)
{
    if (!s || !*s || strlen(s) > 255) return 0;
    for (const unsigned char *p = (const unsigned char *)s; *p; ++p)
        if (!( (*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
               (*p >= '0' && *p <= '9') || *p == '_' || *p == '-' ||
               *p == '.' || *p == ':')) return 0;
    return 1;
}

static int sync_dir(const char *path)
{
    int fd = open(path, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return -1;
    int rc = fsync(fd);
    close(fd);
    return rc;
}

static int write_all(int fd, const char *bytes, size_t count)
{
    size_t done = 0;
    while (done < count) {
        ssize_t n = write(fd, bytes + done, count - done);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return -1;
        done += (size_t)n;
    }
    return 0;
}

static int ensure_metadata(const char *root, char *dir, size_t cap)
{
    int n = snprintf(dir, cap, "%s/.fx-metadata", root);
    if (n < 0 || (size_t)n >= cap) return -1;
    int created = mkdir(dir, 0700) == 0;
    if (!created && errno != EEXIST) return -1;
    struct stat st;
    if (lstat(dir, &st) != 0 || !S_ISDIR(st.st_mode) || S_ISLNK(st.st_mode))
        return -1;
    if (created && sync_dir(root) != 0) return -1;
    return 0;
}

static int fields(char *row, char **out, int count)
{
    int n = 0;
    out[n++] = row;
    for (char *p = row; *p; ++p) {
        if (*p == '|') {
            *p = '\0';
            if (n >= count) return -1;
            out[n++] = p + 1;
        }
    }
    return n;
}

static int write_snapshot(const char *dir, uint64_t epoch, char **rows, size_t nrow)
{
    char path[PATH_MAX], temp[PATH_MAX], header[64];
    static unsigned long serial;
    int fd = -1, n, rc = -1;
    n = snprintf(path, sizeof(path), "%s/leases", dir);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    for (int attempt = 0; attempt < 32; ++attempt) {
        unsigned long ticket = __sync_add_and_fetch(&serial, 1);
        n = snprintf(temp, sizeof(temp), "%s/.leases.%llu.%ld.%lu", dir,
                     (unsigned long long)epoch, (long)getpid(), ticket);
        if (n < 0 || (size_t)n >= sizeof(temp)) return -1;
        fd = open(temp, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC | O_NOFOLLOW,
                  0600);
        if (fd >= 0 || errno != EEXIST) break;
    }
    if (fd < 0) return -1;
    n = snprintf(header, sizeof(header), "fxleases1|%llu\n",
                 (unsigned long long)epoch);
    if (n <= 0 || write_all(fd, header, (size_t)n) != 0) goto done;
    for (size_t i = 0; i < nrow; ++i) {
        size_t len = strlen(rows[i]);
        if (write_all(fd, rows[i], len) != 0) goto done;
    }
    if (fsync(fd) != 0) goto done;
    if (close(fd) != 0) { fd = -1; goto failed; }
    fd = -1;
    if (rename(temp, path) != 0 || sync_dir(dir) != 0) goto failed;
    return 0;
done:
    close(fd);
failed:
    return rc;
}

static int read_snapshot(const char *dir, uint64_t *epoch, char ***rows,
                         size_t *nrow)
{
    char path[PATH_MAX], *data = NULL, *line, *save;
    struct stat st;
    int n = snprintf(path, sizeof(path), "%s/leases", dir);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) {
        if (errno != ENOENT) return -1;
        *epoch = 0; *rows = NULL; *nrow = 0;
        return 0;
    }
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode) || st.st_size < 0 ||
        st.st_size > SNAPSHOT_LIMIT) { close(fd); return -1; }
    size_t size = (size_t)st.st_size;
    data = malloc(size + 1);
    if (!data) { close(fd); return -1; }
    size_t got = 0;
    while (got < size) {
        ssize_t r = read(fd, data + got, size - got);
        if (r < 0 && errno == EINTR) continue;
        if (r <= 0) { free(data); close(fd); return -1; }
        got += (size_t)r;
    }
    close(fd); data[size] = '\0';
    line = strtok_r(data, "\n", &save);
    unsigned long long parsed;
    if (!line || sscanf(line, "fxleases1|%llu", &parsed) != 1) {
        free(data); return -1;
    }
    *epoch = (uint64_t)parsed;
    *rows = calloc(ROW_LIMIT, sizeof(char *));
    if (!*rows) { free(data); return -1; }
    *nrow = 0;
    while ((line = strtok_r(NULL, "\n", &save)) != NULL) {
        if (*nrow >= ROW_LIMIT || strlen(line) > 1024) goto invalid;
        size_t line_len = strlen(line);
        (*rows)[*nrow] = malloc(line_len + 2);
        if (!(*rows)[*nrow]) goto invalid;
        memcpy((*rows)[*nrow], line, line_len);
        (*rows)[*nrow][line_len] = '\n';
        (*rows)[*nrow][line_len + 1] = '\0';
        ++*nrow;
    }
    free(data);
    return 0;
invalid:
    for (size_t i = 0; i < *nrow; ++i) free((*rows)[i]);
    free(*rows); free(data); *rows = NULL; *nrow = 0;
    return -1;
}

static int same_root(char *row, const char *owner, const char *start,
                     const char *reason)
{
    char *f[7];
    if (fields(row, f, 7) != 7) return 0;
    return strcmp(f[0], "R") == 0 && strcmp(f[2], owner) == 0 &&
           strcmp(f[3], start) == 0 && strcmp(f[4], reason) == 0;
}

/* op: 1=set roots, 2=release reason, 3=read lease, 4=release lease,
 *     5=publication lease, 6=commit roots and publication lease. */
int fx_immutable_lease_update(const char *root, int op, const char *owner,
        const char *start, const char *reason, const char *token,
        const char *kind, const char *id, const char *root_rows,
        char *out_token, size_t out_cap, long long *out_epoch)
{
    char dir[PATH_MAX], lock_path[PATH_MAX];
    int lock = -1, n, result = -1, changed = 0, publication_found = 0;
    size_t old_root_count = 0, new_root_count = 0;
    int root_changed = 1;
    uint64_t epoch = 0;
    char **rows = NULL, **next = NULL;
    size_t nrow = 0, nnext = 0;
    if (!root || !safe_field(owner) || !safe_field(start) ||
        (reason && *reason && !safe_field(reason))) return -1;
    if (op < 1 || op > 6) return -1;
    if ((op == 3 && (!safe_field(kind) || !safe_field(id))) ||
        (op == 4 && !safe_field(token)) ||
        (op == 6 && !safe_field(token))) return -1;
    if (ensure_metadata(root, dir, sizeof(dir)) != 0) return -1;
    n = snprintf(lock_path, sizeof(lock_path), "%s/lock", dir);
    if (n < 0 || (size_t)n >= sizeof(lock_path)) return -1;
    lock = open(lock_path, O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (lock < 0 || flock(lock, LOCK_EX) != 0) goto done;
    if (read_snapshot(dir, &epoch, &rows, &nrow) != 0) goto done;
    next = calloc(ROW_LIMIT, sizeof(char *));
    if (!next) goto done;
    for (size_t i = 0; i < nrow; ++i) {
        int remove = 0, root_remove = 0;
        if (op == 1 || op == 2 || op == 6) {
            char copy[1100];
            if (strlen(rows[i]) >= sizeof(copy)) goto done;
            strcpy(copy, rows[i]);
            remove = same_root(copy, owner, start, reason);
            root_remove = remove;
        }
        if (op == 2 || op == 6) {
            char copy[1100], *f[7];
            if (strlen(rows[i]) >= sizeof(copy)) goto done;
            strcpy(copy, rows[i]);
            if (fields(copy, f, 7) == 7 && strcmp(f[0], "P") == 0 &&
                strcmp(f[1], token) == 0 && strcmp(f[2], owner) == 0 &&
                strcmp(f[3], start) == 0) {
                remove = 1;
                publication_found = 1;
            }
        }
        if (op == 4) {
            char copy[1100], *f[7];
            if (strlen(rows[i]) >= sizeof(copy)) goto done;
            strcpy(copy, rows[i]);
            if (fields(copy, f, 7) == 7 && (strcmp(f[0], "L") == 0 ||
                strcmp(f[0], "P") == 0) && strcmp(f[1], token) == 0 &&
                strcmp(f[2], owner) == 0 && strcmp(f[3], start) == 0)
                remove = 1;
        }
        if (remove) {
            if (root_remove) {
                ++old_root_count;
                if (op != 1 && op != 6) changed = 1;
            }
            else changed = 1;
        } else {
            next[nnext] = strdup(rows[i]);
            if (!next[nnext++]) goto done;
        }
    }
    if (op == 1 || op == 6) {
        if (!reason || !*reason || !root_rows) goto done;
        const char *p = root_rows;
        while (*p) {
            char rkind[256], rid[256], line[1100];
            int used = 0;
            if (sscanf(p, "%255[^:]:%255[0-9a-f]%n", rkind, rid, &used) != 2 ||
                used <= 0 || strlen(rid) != 64 || !safe_field(rkind)) goto done;
            for (int k = 0; k < 64; ++k)
                if (!((rid[k] >= '0' && rid[k] <= '9') ||
                      (rid[k] >= 'a' && rid[k] <= 'f'))) goto done;
            n = snprintf(line, sizeof(line), "R||%s|%s|%s|%s|%s\n",
                         owner, start, reason, rkind, rid);
            if (n < 0 || (size_t)n >= sizeof(line) || nnext >= ROW_LIMIT)
                goto done;
            int duplicate = 0;
            for (size_t j = 0; j < nnext; ++j)
                if (strcmp(next[j], line) == 0) duplicate = 1;
            if (!duplicate) {
                next[nnext] = strdup(line);
                if (!next[nnext++]) goto done;
                ++new_root_count;
            }
            p += used;
            if (*p == '\n') ++p;
            else if (*p != '\0') goto done;
        }
    }
    if (op == 3 || op == 5) {
        if (epoch >= (uint64_t)INT64_MAX || out_cap < 32) goto done;
        n = snprintf(out_token, out_cap, "%c%llu", op == 3 ? 'L' : 'P',
                     (unsigned long long)(epoch + 1));
        if (n < 0 || (size_t)n >= out_cap) goto done;
        char line[1100];
        if (op == 3) {
            n = snprintf(line, sizeof(line), "L|%s|%s|%s|%s|%s|%s\n",
                         out_token, owner, start, reason ? reason : "read",
                         kind, id);
            if (n < 0 || (size_t)n >= sizeof(line) || nnext >= ROW_LIMIT)
                goto done;
            next[nnext] = strdup(line);
            if (!next[nnext++]) goto done;
        } else {
            if (!reason || !*reason || !root_rows) goto done;
            const char *p = root_rows;
            while (*p) {
                char rkind[256], rid[256], line[1100];
                int used = 0;
                if (sscanf(p, "%255[^:]:%255[0-9a-f]%n", rkind, rid, &used) != 2 ||
                    used <= 0 || strlen(rid) != 64 || !safe_field(rkind)) goto done;
                for (int k = 0; k < 64; ++k)
                    if (!((rid[k] >= '0' && rid[k] <= '9') ||
                          (rid[k] >= 'a' && rid[k] <= 'f'))) goto done;
                n = snprintf(line, sizeof(line), "P|%s|%s|%s|%s|%s|%s\n",
                             out_token, owner, start, reason, rkind, rid);
                if (n < 0 || (size_t)n >= sizeof(line) || nnext >= ROW_LIMIT)
                    goto done;
                next[nnext] = strdup(line);
                if (!next[nnext++]) goto done;
                p += used;
                if (*p == '\n') ++p;
                else if (*p != '\0') goto done;
            }
            if (nnext == 0) goto done;
        }
        changed = 1;
    }
    if (op == 4 && !changed) { result = 1; goto done; }
    if (op == 6 && !publication_found) goto done;
    if (op == 1 || op == 6) {
        root_changed = old_root_count != new_root_count;
        if (!root_changed) {
            for (size_t i = 0; i < nrow && !root_changed; ++i) {
                char copy[1100];
                if (strlen(rows[i]) >= sizeof(copy)) goto done;
                strcpy(copy, rows[i]);
                if (!same_root(copy, owner, start, reason)) continue;
                for (size_t j = 0; j < nnext; ++j) {
                    if (strcmp(rows[i], next[j]) == 0) break;
                    if (j + 1 == nnext) root_changed = 1;
                }
            }
        }
        if (op == 1) changed = root_changed;
        else if (root_changed) changed = 1;
    }
    if (changed) {
        if (epoch >= (uint64_t)INT64_MAX) goto done;
        ++epoch;
        if (write_snapshot(dir, epoch, next, nnext) != 0) goto done;
    }
    if (out_epoch) *out_epoch = (long long)epoch;
    result = 0;
done:
    if (rows) { for (size_t i = 0; i < nrow; ++i) free(rows[i]); free(rows); }
    if (next) { for (size_t i = 0; i < nnext; ++i) free(next[i]); free(next); }
    if (lock >= 0) { (void)flock(lock, LOCK_UN); close(lock); }
    return result;
}
