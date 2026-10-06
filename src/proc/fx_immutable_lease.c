#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#else
#define _POSIX_C_SOURCE 200809L
#endif
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
/* The snapshot byte limit is the effective bound on valid row counts. */
#define ROW_LIMIT 262144
#define ROW_TEXT_MAX 1100
#define COMPACT_GROUP_MIN 8
#define SNAPSHOT_WRITE_BUFFER (64 * 1024)

typedef struct {
    char bytes[SNAPSHOT_WRITE_BUFFER];
    size_t used;
} snapshot_writer_t;

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

typedef struct {
    char type[2];
    char token[256];
    char owner[256];
    char start[256];
    char reason[256];
    char kind[256];
    char object_id[256];
} lease_record_t;

typedef struct {
    uint16_t offset[7];
    uint16_t length[7];
    uint16_t text_length;
    uint8_t count;
    uint8_t valid;
    uint8_t object_identity;
} lease_row_info_t;

static int safe_field_slice(const char *s, size_t length)
{
    if (!s || length == 0 || length > 255) return 0;
    for (size_t i = 0; i < length; ++i) {
        unsigned char c = (unsigned char)s[i];
        if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
              (c >= '0' && c <= '9') || c == '_' || c == '-' || c == '.' ||
              c == ':')) return 0;
    }
    return 1;
}

static void index_lease_row_fields(const char *row, lease_row_info_t *info)
{
    size_t length = strnlen(row, ROW_TEXT_MAX), start = 0;
    unsigned int count = 0;

    memset(info, 0, sizeof(*info));
    info->text_length = (uint16_t)length;
    if (length >= ROW_TEXT_MAX) return;
    if (length > 0 && row[length - 1] == '\n') --length;
    for (size_t i = 0; i <= length; ++i) {
        if (i != length && row[i] != '|') continue;
        if (count < 7) {
            info->offset[count] = (uint16_t)start;
            info->length[count] = (uint16_t)(i - start);
        }
        if (count < 8) ++count;
        start = i + 1;
    }
    info->count = (uint8_t)count;
}

static void index_lease_row(const char *row, lease_row_info_t *info)
{
    index_lease_row_fields(row, info);
    if (info->count != 7) return;
    if (info->length[0] != 1) return;
    char type = row[info->offset[0]];
    if (type != 'R' && type != 'P' && type != 'L') return;
    for (int i = 1; i < 7; ++i) {
        size_t field_length = info->length[i];
        if (field_length >= sizeof(((lease_record_t *)0)->token)) return;
        if (field_length == 0) {
            if (i != 1 || type != 'R') return;
        } else if (!safe_field_slice(row + info->offset[i], field_length)) {
            return;
        }
    }
    info->valid = 1;
    if (info->length[6] != 64) return;
    for (int i = 0; i < 64; ++i) {
        char c = row[info->offset[6] + i];
        if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))) return;
    }
    info->object_identity = 1;
}

static int row_field_equal(const char *row, const lease_row_info_t *info,
                           int field, const char *value)
{
    size_t length = strlen(value);
    return info->count == 7 && info->length[field] == length &&
           memcmp(row + info->offset[field], value, length) == 0;
}

static int copy_row_field(char *destination, size_t capacity, const char *row,
                          const lease_row_info_t *info, int field)
{
    size_t length = info->length[field];
    if (info->count != 7 || length >= capacity) return -1;
    memcpy(destination, row + info->offset[field], length);
    destination[length] = '\0';
    return 0;
}

static int same_row_group(const char *left, const lease_row_info_t *left_info,
                          const char *right,
                          const lease_row_info_t *right_info)
{
    if (!left_info->valid || !right_info->valid) return 0;
    for (int i = 0; i < 6; ++i) {
        if (left_info->length[i] != right_info->length[i] ||
            memcmp(left + left_info->offset[i], right + right_info->offset[i],
                   left_info->length[i]) != 0) return 0;
    }
    return 1;
}

static int copy_field(char *destination, size_t capacity, const char *source,
                      int allow_empty)
{
    size_t length = strlen(source);
    if (length >= capacity || (!allow_empty && !safe_field(source)) ||
        (allow_empty && length > 0 && !safe_field(source))) return -1;
    memcpy(destination, source, length + 1);
    return 0;
}

static int flush_snapshot(snapshot_writer_t *writer, int fd)
{
    if (writer->used == 0) return 0;
    if (write_all(fd, writer->bytes, writer->used) != 0) return -1;
    writer->used = 0;
    return 0;
}

static int write_snapshot_record(snapshot_writer_t *writer, int fd,
                                 const char *record, size_t *total)
{
    size_t length = strlen(record);
    if (*total > SNAPSHOT_LIMIT || length > (size_t)SNAPSHOT_LIMIT - *total)
        return -1;
    if (length > sizeof(writer->bytes)) return -1;
    if (length > sizeof(writer->bytes) - writer->used &&
        flush_snapshot(writer, fd) != 0) return -1;
    memcpy(writer->bytes + writer->used, record, length);
    writer->used += length;
    *total += length;
    return 0;
}

static int parse_compact_group(char *line, lease_record_t *group)
{
    char copy[ROW_TEXT_MAX], *part[7];
    size_t length = strlen(line);

    if (length >= sizeof(copy)) return -1;
    memcpy(copy, line, length + 1);
    if (length > 0 && copy[length - 1] == '\n') copy[--length] = '\0';
    if (fields(copy, part, 7) != 7 || strcmp(part[0], "G") != 0 ||
        strlen(part[1]) != 1 ||
        (strcmp(part[1], "R") != 0 && strcmp(part[1], "P") != 0 &&
         strcmp(part[1], "L") != 0)) return -1;
    if (copy_field(group->type, sizeof(group->type), part[1], 0) != 0 ||
        copy_field(group->token, sizeof(group->token), part[2],
                   strcmp(part[1], "R") == 0) != 0 ||
        copy_field(group->owner, sizeof(group->owner), part[3], 0) != 0 ||
        copy_field(group->start, sizeof(group->start), part[4], 0) != 0 ||
        copy_field(group->reason, sizeof(group->reason), part[5], 0) != 0 ||
        copy_field(group->kind, sizeof(group->kind), part[6], 0) != 0)
        return -1;
    return 0;
}

static int parse_compact_object(const char *line, char object_id[65])
{
    if (strlen(line) != 66 || line[0] != 'O' || line[1] != '|') return -1;
    for (int i = 0; i < 64; ++i) {
        char value = line[i + 2];
        if (!((value >= '0' && value <= '9') ||
              (value >= 'a' && value <= 'f'))) return -1;
        object_id[i] = value;
    }
    object_id[64] = '\0';
    return 0;
}

static int append_snapshot_row(char ***rows, lease_row_info_t *info,
                               size_t *nrow, const char *line, size_t length,
                               int compact_member)
{
    if (*nrow >= ROW_LIMIT || length >= ROW_TEXT_MAX) return -1;
    (*rows)[*nrow] = malloc(length + 2);
    if (!(*rows)[*nrow]) return -1;
    memcpy((*rows)[*nrow], line, length);
    (*rows)[*nrow][length] = '\n';
    (*rows)[*nrow][length + 1] = '\0';
    if (compact_member) {
        index_lease_row_fields((*rows)[*nrow], &info[*nrow]);
        if (info[*nrow].count != 7 || info[*nrow].length[6] != 64) {
            free((*rows)[*nrow]);
            (*rows)[*nrow] = NULL;
            return -1;
        }
        info[*nrow].valid = 1;
        info[*nrow].object_identity = 1;
    } else {
        index_lease_row((*rows)[*nrow], &info[*nrow]);
    }
    ++*nrow;
    return 0;
}

static int write_snapshot(const char *dir, uint64_t epoch, char **rows,
                          lease_row_info_t *info, size_t nrow)
{
    char path[PATH_MAX], temp[PATH_MAX], header[64];
    static unsigned long serial;
    int fd = -1, n, rc = -1;
    size_t total = 0;
    snapshot_writer_t writer = { .used = 0 };
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
    n = snprintf(header, sizeof(header), "fxleases2|%llu\n",
                 (unsigned long long)epoch);
    if (n <= 0 || (size_t)n >= sizeof(header) ||
        write_snapshot_record(&writer, fd, header, &total) != 0) goto done;
    for (size_t i = 0; i < nrow;) {
        lease_record_t first;
        size_t end = i + 1;
        int compact = info[i].valid && info[i].object_identity;
        if (compact) {
            while (end < nrow) {
                if (!info[end].valid || !info[end].object_identity ||
                    !same_row_group(rows[i], &info[i], rows[end],
                                    &info[end])) break;
                ++end;
            }
        }
        if (compact && end - i >= COMPACT_GROUP_MIN) {
            char record[ROW_TEXT_MAX];
            if (copy_row_field(first.type, sizeof(first.type), rows[i],
                               &info[i], 0) != 0 ||
                copy_row_field(first.token, sizeof(first.token), rows[i],
                               &info[i], 1) != 0 ||
                copy_row_field(first.owner, sizeof(first.owner), rows[i],
                               &info[i], 2) != 0 ||
                copy_row_field(first.start, sizeof(first.start), rows[i],
                               &info[i], 3) != 0 ||
                copy_row_field(first.reason, sizeof(first.reason), rows[i],
                               &info[i], 4) != 0 ||
                copy_row_field(first.kind, sizeof(first.kind), rows[i],
                               &info[i], 5) != 0) goto done;
            n = snprintf(record, sizeof(record), "G|%s|%s|%s|%s|%s|%s\n",
                         first.type, first.token, first.owner, first.start,
                         first.reason, first.kind);
            if (n < 0 || (size_t)n >= sizeof(record) ||
                write_snapshot_record(&writer, fd, record, &total) != 0) goto done;
            for (size_t j = i; j < end; ++j) {
                char object_id[65];
                if (copy_row_field(object_id, sizeof(object_id), rows[j],
                                   &info[j], 6) != 0) goto done;
                n = snprintf(record, sizeof(record), "O|%s\n", object_id);
                if (n < 0 || (size_t)n >= sizeof(record) ||
                    write_snapshot_record(&writer, fd, record, &total) != 0) goto done;
            }
            i = end;
        } else {
            if (write_snapshot_record(&writer, fd, rows[i], &total) != 0)
                goto done;
            ++i;
        }
    }
    if (flush_snapshot(&writer, fd) != 0 || fsync(fd) != 0) goto done;
    if (close(fd) != 0) { fd = -1; goto failed; }
    fd = -1;
    if (rename(temp, path) != 0 || sync_dir(dir) != 0) goto failed;
    return 0;
done:
    close(fd);
failed:
    unlink(temp);
    return rc;
}

static int read_snapshot(const char *dir, uint64_t *epoch, char ***rows,
                         lease_row_info_t **info, size_t *nrow)
{
    char path[PATH_MAX], *data = NULL, *line, *save;
    lease_record_t group;
    size_t group_count = 0;
    int compact_format = 0, group_active = 0;
    struct stat st;
    int n = snprintf(path, sizeof(path), "%s/leases", dir);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) {
        if (errno != ENOENT) return -1;
        *epoch = 0; *rows = NULL; *info = NULL; *nrow = 0;
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
    char *epoch_text = NULL, *epoch_end = NULL;
    if (line && strncmp(line, "fxleases1|", 10) == 0) {
        epoch_text = line + 10;
    } else if (line && strncmp(line, "fxleases2|", 10) == 0) {
        compact_format = 1;
        epoch_text = line + 10;
    }
    if (!epoch_text || !*epoch_text) {
        free(data); return -1;
    }
    errno = 0;
    parsed = strtoull(epoch_text, &epoch_end, 10);
    if (errno != 0 || !epoch_end || *epoch_end != '\0') {
        free(data); return -1;
    }
    *epoch = (uint64_t)parsed;
    *rows = calloc(ROW_LIMIT, sizeof(char *));
    if (!*rows) { free(data); return -1; }
    *info = calloc(ROW_LIMIT, sizeof(lease_row_info_t));
    if (!*info) { free(*rows); *rows = NULL; free(data); return -1; }
    *nrow = 0;
    while ((line = strtok_r(NULL, "\n", &save)) != NULL) {
        size_t line_len = strlen(line);
        if (line_len >= ROW_TEXT_MAX) goto invalid;
        if (compact_format && strncmp(line, "G|", 2) == 0) {
            if ((group_active && group_count < COMPACT_GROUP_MIN) ||
                parse_compact_group(line, &group) != 0) goto invalid;
            group_active = 1;
            group_count = 0;
            continue;
        }
        if (compact_format && strncmp(line, "O|", 2) == 0) {
            char object_id[65], expanded[ROW_TEXT_MAX];
            if (!group_active || parse_compact_object(line, object_id) != 0)
                goto invalid;
            int expanded_len = snprintf(expanded, sizeof(expanded),
                "%s|%s|%s|%s|%s|%s|%s", group.type, group.token,
                group.owner, group.start, group.reason, group.kind, object_id);
            if (expanded_len < 0 || (size_t)expanded_len >= sizeof(expanded) ||
                append_snapshot_row(rows, *info, nrow, expanded,
                                    (size_t)expanded_len, 1) != 0) goto invalid;
            ++group_count;
            continue;
        }
        if (group_active) {
            if (group_count < COMPACT_GROUP_MIN) goto invalid;
            group_active = 0;
        }
        if (append_snapshot_row(rows, *info, nrow, line, line_len, 0) != 0)
            goto invalid;
    }
    if (group_active && group_count < COMPACT_GROUP_MIN) goto invalid;
    free(data);
    return 0;
invalid:
    for (size_t i = 0; i < *nrow; ++i) free((*rows)[i]);
    free(*rows); free(*info); free(data);
    *rows = NULL; *info = NULL; *nrow = 0;
    return -1;
}

static int same_root(const char *row, const lease_row_info_t *info,
                     const char *owner, const char *start, const char *reason)
{
    return row_field_equal(row, info, 0, "R") &&
           row_field_equal(row, info, 2, owner) &&
           row_field_equal(row, info, 3, start) &&
           row_field_equal(row, info, 4, reason);
}

static int append_next_row(char **rows, lease_row_info_t *info, size_t *count,
                           const char *line, const lease_row_info_t *source_info)
{
    if (*count >= ROW_LIMIT) return -1;
    rows[*count] = strdup(line);
    if (!rows[*count]) return -1;
    if (source_info) info[*count] = *source_info;
    else index_lease_row(rows[*count], &info[*count]);
    ++*count;
    return 0;
}

/* op: 1=set roots, 2=release reason, 3=read lease, 4=release lease,
 *     5=publication lease, 6=commit roots and publication lease,
 *     7=lease all roots/publications for an owner graph. */
int fx_immutable_lease_update(const char *root, int op, const char *owner,
        const char *start, const char *reason, const char *token,
        const char *kind, const char *id, const char *root_rows,
        char *out_token, size_t out_cap, long long *out_epoch)
{
    char dir[PATH_MAX], lock_path[PATH_MAX];
    int lock = -1, n, result = -1, changed = 0, publication_found = 0;
    int graph_found = 0;
    size_t old_root_count = 0, new_root_count = 0;
    int root_changed = 1;
    uint64_t epoch = 0;
    char **rows = NULL, **next = NULL;
    lease_row_info_t *row_info = NULL, *next_info = NULL;
    size_t nrow = 0, nnext = 0;
    if (!root || !safe_field(owner) || !safe_field(start) ||
        (reason && *reason && !safe_field(reason))) return -1;
    if (op < 1 || op > 7) return -1;
    if ((op == 3 && (!safe_field(kind) || !safe_field(id))) ||
        (op == 4 && !safe_field(token)) ||
        (op == 6 && !safe_field(token)) ||
        (op == 7 && (!safe_field(reason) || !out_token || out_cap < 32)))
        return -1;
    if (ensure_metadata(root, dir, sizeof(dir)) != 0) return -1;
    n = snprintf(lock_path, sizeof(lock_path), "%s/lock", dir);
    if (n < 0 || (size_t)n >= sizeof(lock_path)) return -1;
    lock = open(lock_path, O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (lock < 0 || flock(lock, LOCK_EX) != 0) goto done;
    if (read_snapshot(dir, &epoch, &rows, &row_info, &nrow) != 0) goto done;
    next = calloc(ROW_LIMIT, sizeof(char *));
    next_info = calloc(ROW_LIMIT, sizeof(lease_row_info_t));
    if (!next || !next_info) goto done;
    if (op == 7) {
        if (epoch >= (uint64_t)INT64_MAX) goto done;
        n = snprintf(out_token, out_cap, "L%llu",
                     (unsigned long long)(epoch + 1));
        if (n < 0 || (size_t)n >= out_cap) goto done;
    }
    for (size_t i = 0; i < nrow; ++i) {
        int remove = 0, root_remove = 0;
        if ((op == 1 || op == 2 || op == 4 || op == 6 || op == 7) &&
            row_info[i].text_length >= ROW_TEXT_MAX) goto done;
        if (op == 1 || op == 2 || op == 6) {
            remove = same_root(rows[i], &row_info[i], owner, start, reason);
            root_remove = remove;
        }
        if (op == 2 || op == 6) {
            if (row_field_equal(rows[i], &row_info[i], 0, "P") &&
                row_field_equal(rows[i], &row_info[i], 1, token) &&
                row_field_equal(rows[i], &row_info[i], 2, owner) &&
                row_field_equal(rows[i], &row_info[i], 3, start)) {
                remove = 1;
                publication_found = 1;
            }
        }
        if (op == 4) {
            if ((row_field_equal(rows[i], &row_info[i], 0, "L") ||
                 row_field_equal(rows[i], &row_info[i], 0, "P")) &&
                row_field_equal(rows[i], &row_info[i], 1, token) &&
                row_field_equal(rows[i], &row_info[i], 2, owner) &&
                row_field_equal(rows[i], &row_info[i], 3, start))
                remove = 1;
        }
        if (op == 7) {
            char line[1100], kind_field[ROW_TEXT_MAX], object_id[ROW_TEXT_MAX];
            if ((row_field_equal(rows[i], &row_info[i], 0, "R") ||
                 row_field_equal(rows[i], &row_info[i], 0, "P")) &&
                row_field_equal(rows[i], &row_info[i], 2, owner) &&
                row_field_equal(rows[i], &row_info[i], 3, start)) {
                graph_found = 1;
                if (copy_row_field(kind_field, sizeof(kind_field), rows[i],
                                   &row_info[i], 5) != 0 ||
                    copy_row_field(object_id, sizeof(object_id), rows[i],
                                   &row_info[i], 6) != 0) goto done;
                n = snprintf(line, sizeof(line), "L|%s|%s|%s|%s|%s|%s\n",
                             out_token, owner, start, reason, kind_field,
                             object_id);
                if (n < 0 || (size_t)n >= sizeof(line)) goto done;
                int duplicate = 0;
                for (size_t j = 0; j < nnext; ++j)
                    if (strcmp(next[j], line) == 0) duplicate = 1;
                if (!duplicate) {
                    if (append_next_row(next, next_info, &nnext, line, NULL) != 0)
                        goto done;
                    changed = 1;
                }
            }
        }
        if (remove) {
            if (root_remove) {
                ++old_root_count;
                if (op != 1 && op != 6) changed = 1;
            }
            else changed = 1;
        } else {
            if (append_next_row(next, next_info, &nnext, rows[i],
                                &row_info[i]) != 0) goto done;
        }
    }
    if (op == 7 && !graph_found) { result = 1; goto done; }
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
                if (append_next_row(next, next_info, &nnext, line, NULL) != 0)
                    goto done;
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
            if (append_next_row(next, next_info, &nnext, line, NULL) != 0)
                goto done;
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
                if (append_next_row(next, next_info, &nnext, line, NULL) != 0)
                    goto done;
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
                if (!same_root(rows[i], &row_info[i], owner, start, reason))
                    continue;
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
        if (write_snapshot(dir, epoch, next, next_info, nnext) != 0) goto done;
    }
    if (out_epoch) *out_epoch = (long long)epoch;
    result = 0;
done:
    if (rows) { for (size_t i = 0; i < nrow; ++i) free(rows[i]); free(rows); }
    if (next) { for (size_t i = 0; i < nnext; ++i) free(next[i]); free(next); }
    free(row_info);
    free(next_info);
    if (lock >= 0) { (void)flock(lock, LOCK_UN); close(lock); }
    return result;
}
