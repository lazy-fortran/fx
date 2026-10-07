#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#else
#define _POSIX_C_SOURCE 200809L
#endif
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
#define SNAPSHOT_LIMIT (16 * 1024 * 1024)
#define RECORD_LIMIT 512

typedef struct {
    char id[65];
    char kind;
    off_t size;
    long long allocated;
    time_t mtime;
    dev_t dev;
    ino_t ino;
    int marked;
} object_t;
typedef struct {
    const char *root;
    object_t *objects, **index, **queue;
    size_t count, capacity, queued, entries_seen, tree_bytes;
    long long total;
    size_t remaining;
    long long epoch;
    int lock;
} gc_t;

static int valid_id(const char *id)
{
    if (!id || strlen(id) != 64) return 0;
    for (int i = 0; i < 64; ++i)
        if (!((id[i] >= '0' && id[i] <= '9') ||
              (id[i] >= 'a' && id[i] <= 'f'))) return 0;
    return 1;
}


static int object_order(const void *left, const void *right)
{
    const object_t *const *a = left, *const *b = right;
    if ((*a)->kind != (*b)->kind) return (*a)->kind < (*b)->kind ? -1 : 1;
    return strcmp((*a)->id, (*b)->id);
}

static object_t *find_object(gc_t *g, char kind, const char *id)
{
    size_t lo = 0, hi = g->count;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        object_t *o = g->index[mid];
        int cmp = o->kind == kind ? strcmp(o->id, id) :
                  (o->kind < kind ? -1 : 1);
        if (cmp < 0) lo = mid + 1;
        else hi = mid;
    }
    if (lo < g->count && g->index[lo]->kind == kind &&
        strcmp(g->index[lo]->id, id) == 0) return g->index[lo];
    return NULL;
}

static int mark(gc_t *g, const char *kind, const char *id)
{
    char code;
    if (strcmp(kind, "blob") == 0) code = 'B';
    else if (strcmp(kind, "tree") == 0) code = 'T';
    else return -1;
    if (!valid_id(id)) return -1;
    object_t *o = find_object(g, code, id);
    /* A pending publication may name an object which has not appeared yet. */
    if (!o) return 0;
    if (!o->marked && code == 'T') g->queue[g->queued++] = o;
    o->marked = 1;
    return 0;
}

static int open_kind_dir(const char *root, char kind)
{
    int base = open(root, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (base < 0) return -1;
    int first = openat(base, kind == 'B' ? "blobs" : "trees",
                       O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    close(base);
    if (first < 0) return -1;
    int sha = openat(first, "sha256",
                     O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    close(first);
    return sha;
}

static int scan_shard(gc_t *g, char kind, int parent, const char *shard)
{
    int fd = openat(parent, shard,
                    O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return -1;
    DIR *dir = fdopendir(fd);
    if (!dir) { close(fd); return -1; }
    int rc = 0;
    struct dirent *e;
    for (;;) {
        errno = 0;
        e = readdir(dir);
        if (!e) { if (errno) rc = -1; break; }
        struct stat st;
        if (++g->entries_seen > 4 * g->capacity + 1024) {
            rc = -1; break;
        }
        if (e->d_name[0] == '.') {
            if (strcmp(e->d_name, ".") == 0 ||
                strcmp(e->d_name, "..") == 0 ||
                strncmp(e->d_name, ".fx-owned-", 10) == 0) continue;
            rc = -1; break;
        }
        if (!valid_id(e->d_name) || strncmp(e->d_name, shard, 2) != 0 ||
            fstatat(fd, e->d_name, &st, AT_SYMLINK_NOFOLLOW) != 0 ||
            !S_ISREG(st.st_mode) || (st.st_mode & 0222) ||
            st.st_size < 0 ||
            g->count >= g->capacity || st.st_blocks < 0 ||
            st.st_blocks > LLONG_MAX / 512 ||
            g->total > LLONG_MAX - st.st_blocks * 512) {
            rc = -1; break;
        }
        object_t *o = &g->objects[g->count++];
        memcpy(o->id, e->d_name, 65);
        o->kind = kind;
        o->size = st.st_size;
        o->allocated = st.st_blocks * 512;
        o->mtime = st.st_mtime;
        o->dev = st.st_dev;
        o->ino = st.st_ino;
        o->marked = 0;
        g->total += o->allocated;
    }
    if (closedir(dir) != 0) rc = -1;
    return rc;
}

static int scan_kind(gc_t *g, char kind)
{
    int fd = open_kind_dir(g->root, kind);
    if (fd < 0) return errno == ENOENT ? 0 : -1;
    DIR *dir = fdopendir(fd);
    if (!dir) { close(fd); return -1; }
    struct dirent *e;
    int rc = 0;
    for (;;) {
        errno = 0;
        e = readdir(dir);
        if (!e) { if (errno) rc = -1; break; }
        if (strcmp(e->d_name, ".") == 0 ||
            strcmp(e->d_name, "..") == 0) continue;
        if (++g->entries_seen > 4 * g->capacity + 1024) {
            rc = -1; break;
        }
        if (strlen(e->d_name) != 2 ||
            !((e->d_name[0] >= '0' && e->d_name[0] <= '9') ||
              (e->d_name[0] >= 'a' && e->d_name[0] <= 'f')) ||
            !((e->d_name[1] >= '0' && e->d_name[1] <= '9') ||
              (e->d_name[1] >= 'a' && e->d_name[1] <= 'f')) ||
            scan_shard(g, kind, fd, e->d_name) != 0) { rc = -1; break; }
    }
    if (closedir(dir) != 0) rc = -1;
    return rc;
}

static int open_metadata_dir(const char *root)
{
    int base = open(root, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (base < 0) return -1;
    int meta = openat(base, ".fx-metadata",
                      O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    close(base);
    return meta;
}

static int snapshot(gc_t *g, int roots)
{
    char *line = NULL, group_kind[8] = "";
    int compact = 0, group_active = 0, group_count = 0;
    size_t cap = 0;
    int meta = open_metadata_dir(g->root);
    if (meta < 0) return -1;
    int fd = openat(meta, "leases", O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    int saved_errno = errno;
    close(meta);
    if (fd < 0) return saved_errno == ENOENT ? 0 : -1;
    FILE *f = fdopen(fd, "r");
    if (!f) { close(fd); return -1; }
    struct stat st;
    int rc = -1;
    if (fstat(fileno(f), &st) != 0 || !S_ISREG(st.st_mode) ||
        st.st_size > SNAPSHOT_LIMIT) goto done;
    ssize_t len = getline(&line, &cap, f);
    if (len < 12 || strncmp(line, "fxleases", 8) != 0 ||
        (line[8] != '1' && line[8] != '2') || line[9] != '|') goto done;
    char *end = NULL;
    errno = 0;
    long long epoch = strtoll(line + 10, &end, 10);
    if (errno || !end || *end != '\n' || epoch < 0) goto done;
    compact = line[8] == '2';
    if (!roots) { rc = epoch == g->epoch ? 0 : 1; goto done; }
    g->epoch = epoch;
    while ((len = getline(&line, &cap, f)) >= 0) {
        if (len < 2 || len > 1100 || line[len - 1] != '\n') goto done;
        line[len - 1] = '\0';
        if (line[0] == 'O' && line[1] == '|') {
            if (!group_active || mark(g, group_kind, line + 2) != 0)
                goto done;
            ++group_count;
            continue;
        }
        char *field[8], *cursor = line;
        int nf = 0;
        field[nf++] = cursor;
        while (*cursor) {
            if (*cursor == '|') {
                *cursor = '\0';
                if (nf >= 8) goto done;
                field[nf++] = cursor + 1;
            }
            ++cursor;
        }
        if (group_active && group_count < 8) goto done;
        group_active = 0;
        if (field[0][0] == 'G' && field[0][1] == '\0') {
            if (!compact || nf != 7 ||
                (strcmp(field[1], "R") && strcmp(field[1], "P") &&
                 strcmp(field[1], "L"))) goto done;
            if (strlen(field[6]) >= sizeof(group_kind)) goto done;
            strcpy(group_kind, field[6]);
            group_active = 1;
            group_count = 0;
            continue;
        }
        *group_kind = '\0';
        if (nf != 7 ||
            (strcmp(field[0], "R") && strcmp(field[0], "P") &&
             strcmp(field[0], "L")) || mark(g, field[5], field[6]) != 0)
            goto done;
    }
    if (!ferror(f) && (!group_active || group_count >= 8)) rc = 0;
done:
    free(line);
    fclose(f);
    return rc;
}

static int read_small_fd(int fd, size_t limit, char **out, size_t *size,
                         const object_t *expected)
{
    struct stat st;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode) ||
        st.st_size < 0 || (uintmax_t)st.st_size > limit) return -1;
    if (expected && (st.st_dev != expected->dev || st.st_ino != expected->ino ||
                     st.st_size != expected->size ||
                     st.st_mtime != expected->mtime)) return -1;
    char *data = malloc((size_t)st.st_size + 1);
    if (!data) return -1;
    size_t got = 0;
    while (got < (size_t)st.st_size) {
        ssize_t n = read(fd, data + got, (size_t)st.st_size - got);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) { free(data); return -1; }
        got += (size_t)n;
    }
    data[got] = '\0';
    if (memchr(data, '\0', got)) { free(data); return -1; }
    *out = data;
    *size = got;
    return 0;
}

static int read_small(const char *path, size_t limit, char **out, size_t *size)
{
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return -1;
    int rc = read_small_fd(fd, limit, out, size, NULL);
    close(fd);
    return rc;
}

static int read_tree(gc_t *g, const object_t *tree, char **out, size_t *size)
{
    int parent = open_kind_dir(g->root, 'T');
    if (parent < 0) return -1;
    char shard[3] = {tree->id[0], tree->id[1], '\0'};
    int dir = openat(parent, shard,
                     O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    close(parent);
    if (dir < 0) return -1;
    int fd = openat(dir, tree->id, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    close(dir);
    if (fd < 0) return -1;
    int rc = read_small_fd(fd, 8 * 1024 * 1024, out, size, tree);
    close(fd);
    return rc;
}

static int scan_action_record(gc_t *g, const char *path, const char *id)
{
    char *data = NULL, *save = NULL, *line;
    size_t size;
    int rc = -1;
    if (read_small(path, RECORD_LIMIT, &data, &size) != 0) return -1;
    if (size < 11 || data[size - 1] != '\n') goto done;
    line = strtok_r(data, "\n", &save);
    if (!line || strcmp(line, "FXACTION2") != 0) goto done;
    line = strtok_r(NULL, "\n", &save);
    if (!line || strcmp(line, id) != 0) goto done;
    line = strtok_r(NULL, "\n", &save);
    if (!line) goto done;
    if (strcmp(line, "BOUND") == 0) {
        line = strtok_r(NULL, "\n", &save);
        if (!line || size != 10 + 65 + 6 + 65 ||
            mark(g, "tree", line) != 0) goto done;
    } else if (strcmp(line, "RETIRED") == 0) {
        if (size != 10 + 65 + 8) goto done;
    } else if (strcmp(line, "NONDETERMINISTIC_ACTION incomplete-key") == 0) {
        char first[65];
        line = strtok_r(NULL, "\n", &save);
        if (!line || !valid_id(line)) goto done;
        strcpy(first, line);
        line = strtok_r(NULL, "\n", &save);
        if (!line || size != 10 + 65 +
            sizeof("NONDETERMINISTIC_ACTION incomplete-key") + 65 + 65 ||
            !valid_id(line) || strcmp(first, line) >= 0 ||
            mark(g, "tree", first) != 0 || mark(g, "tree", line) != 0)
            goto done;
    } else goto done;
    if (strtok_r(NULL, "\n", &save) != NULL) goto done;
    rc = 0;
done:
    free(data);
    return rc;
}

static int scan_actions(gc_t *g)
{
    char base[PATH_MAX], shard_path[PATH_MAX], record_path[PATH_MAX];
    int n = snprintf(base, sizeof(base), "%s/actions/sha256", g->root);
    if (n < 0 || (size_t)n >= sizeof(base)) return -1;
    DIR *top = opendir(base);
    if (!top) return errno == ENOENT ? 0 : -1;
    struct dirent *shard, *entry;
    int rc = 0;
    size_t records = 0;
    for (;;) {
        errno = 0;
        shard = readdir(top);
        if (!shard) { if (errno) rc = -1; break; }
        if (!strcmp(shard->d_name, ".") || !strcmp(shard->d_name, ".."))
            continue;
        if (++g->entries_seen > 4 * g->capacity + 1024) {
            rc = -1; break;
        }
        if (strlen(shard->d_name) != 2) { rc = -1; break; }
        n = snprintf(shard_path, sizeof(shard_path), "%s/%s", base,
                     shard->d_name);
        if (n < 0 || (size_t)n >= sizeof(shard_path)) { rc = -1; break; }
        DIR *dir = opendir(shard_path);
        if (!dir) { rc = -1; break; }
        for (;;) {
            errno = 0;
            entry = readdir(dir);
            if (!entry) { if (errno) rc = -1; break; }
            if (++g->entries_seen > 4 * g->capacity + 1024) {
                rc = -1; break;
            }
            if (entry->d_name[0] == '.') {
                if (!strcmp(entry->d_name, ".") ||
                    !strcmp(entry->d_name, "..") ||
                    !strncmp(entry->d_name, ".tmp.", 5)) continue;
                rc = -1; break;
            }
            size_t len = strlen(entry->d_name);
            if (len == 69 && !strcmp(entry->d_name + 64, ".lock")) continue;
            if (!valid_id(entry->d_name) ||
                strncmp(entry->d_name, shard->d_name, 2) != 0 ||
                ++records > g->capacity) { rc = -1; break; }
            n = snprintf(record_path, sizeof(record_path), "%s/%s",
                         shard_path, entry->d_name);
            if (n < 0 || (size_t)n >= sizeof(record_path) ||
                scan_action_record(g, record_path, entry->d_name) != 0) {
                rc = -1; break;
            }
        }
        if (closedir(dir) != 0) rc = -1;
        if (rc) break;
    }
    if (closedir(top) != 0) rc = -1;
    return rc;
}

static int mark_tree_children(gc_t *g, const object_t *tree)
{
    char *data = NULL, *save = NULL, *line;
    size_t size;
    int rc = -1;
    if (read_tree(g, tree, &data, &size) != 0) goto done;
    if (size > 256 * 1024 * 1024 - g->tree_bytes) goto done;
    g->tree_bytes += size;
    if (size < 8 || data[size - 1] != '\n' ||
        memcmp(data, "FXTREE1\n", 8) != 0) goto done;
    line = strtok_r(data + 8, "\n", &save);
    while (line) {
        char *field[5], *cursor = line;
        int nf = 0;
        field[nf++] = cursor;
        while (*cursor) {
            if (*cursor == '\t') {
                *cursor = '\0';
                if (nf >= 5) goto done;
                field[nf++] = cursor + 1;
            }
            ++cursor;
        }
        if (nf != 5 || (strcmp(field[0], "B") && strcmp(field[0], "T")) ||
            !valid_id(field[4])) goto done;
        char kind = field[0][0];
        object_t *child = find_object(g, kind, field[4]);
        if (!child || mark(g, kind == 'B' ? "blob" : "tree", field[4]) != 0)
            goto done;
        line = strtok_r(NULL, "\n", &save);
    }
    rc = 0;
done:
    free(data);
    return rc;
}

static int mark_graph(gc_t *g)
{
    for (size_t i = 0; i < g->queued; ++i)
        if (mark_tree_children(g, g->queue[i]) != 0) return -1;
    return 0;
}

static int oldest_first(const void *left, const void *right)
{
    const object_t *const *a = left, *const *b = right;
    if ((*a)->mtime < (*b)->mtime) return -1;
    if ((*a)->mtime > (*b)->mtime) return 1;
    if ((*a)->kind != (*b)->kind) return (*a)->kind < (*b)->kind ? -1 : 1;
    return strcmp((*a)->id, (*b)->id);
}

static int unlink_object(gc_t *g, const object_t *o)
{
    int parent = open_kind_dir(g->root, o->kind);
    if (parent < 0) return -1;
    char shard[3] = {o->id[0], o->id[1], '\0'};
    int dir = openat(parent, shard,
                     O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    close(parent);
    if (dir < 0) return -1;
    struct stat st;
    int rc = fstatat(dir, o->id, &st, AT_SYMLINK_NOFOLLOW);
    if (rc == 0 && S_ISREG(st.st_mode) && st.st_dev == o->dev &&
        st.st_ino == o->ino && st.st_size == o->size &&
        st.st_mtime == o->mtime) {
        rc = unlinkat(dir, o->id, 0);
        if (rc == 0) rc = fsync(dir);
    } else rc = -1;
    close(dir);
    return rc;
}

/* Explicit, capped maintenance operation. Return 1 when the snapshot changed;
 * caller may retry from a fresh scan. No deletion occurs in that case. */
int fx_immutable_gc_collect(const char *root, int max_scan, int max_delete,
                            long long min_age_seconds, long long pressure_bytes,
                            int pressure_objects, int *scanned,
                            long long *allocated, int *deleted,
                            long long *reclaimed)
{
    gc_t g = {0};
    object_t **candidates = NULL;
    int rc = -1;
    time_t now = time(NULL);
    if (scanned) *scanned = 0;
    if (allocated) *allocated = 0;
    if (deleted) *deleted = 0;
    if (reclaimed) *reclaimed = 0;
    if (!root || !scanned || !allocated || !deleted || !reclaimed ||
        max_scan <= 0 ||
        max_scan > 1000000 || max_delete < 0 || max_delete > max_scan ||
        min_age_seconds < 0 || pressure_bytes < 0 ||
        pressure_objects < 0 || now == (time_t)-1)
        return -2;
    g.root = root;
    g.capacity = (size_t)max_scan;
    g.lock = -1;
    g.objects = calloc(g.capacity, sizeof(object_t));
    if (!g.objects) return -1;
    if (scan_kind(&g, 'B') != 0 || scan_kind(&g, 'T') != 0) goto done;
    g.remaining = g.count;
    *scanned = (int)g.count;
    *allocated = g.total;
    g.index = calloc(g.count ? g.count : 1, sizeof(object_t *));
    g.queue = calloc(g.count ? g.count : 1, sizeof(object_t *));
    if (!g.index || !g.queue) goto done;
    for (size_t i = 0; i < g.count; ++i) g.index[i] = &g.objects[i];
    qsort(g.index, g.count, sizeof(object_t *), object_order);
    if (snapshot(&g, 1) != 0 || scan_actions(&g) != 0 ||
        mark_graph(&g) != 0) goto done;
    if ((g.total <= pressure_bytes &&
         g.remaining <= (size_t)pressure_objects) ||
        max_delete == 0) { rc = 0; goto done; }
    candidates = calloc(g.count ? g.count : 1, sizeof(object_t *));
    if (!candidates) goto done;
    size_t ncandidates = 0;
    for (size_t i = 0; i < g.count; ++i) {
        object_t *o = &g.objects[i];
        if (!o->marked && o->mtime <= now &&
            (long long)(now - o->mtime) >= min_age_seconds)
            candidates[ncandidates++] = o;
    }
    qsort(candidates, ncandidates, sizeof(object_t *), oldest_first);
    int meta = open_metadata_dir(root);
    if (meta < 0) goto done;
    g.lock = openat(meta, "lock", O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW,
                    0600);
    close(meta);
    if (g.lock < 0) goto done;
    while (flock(g.lock, LOCK_EX) != 0)
        if (errno != EINTR) goto done;
    rc = snapshot(&g, 0);
    if (rc != 0) goto done;
    for (size_t i = 0; i < ncandidates && *deleted < max_delete &&
         (g.total > pressure_bytes ||
          g.remaining > (size_t)pressure_objects); ++i) {
        object_t *o = candidates[i];
        if (unlink_object(&g, o) != 0) { rc = -1; break; }
        ++*deleted;
        *reclaimed += o->allocated;
        g.total -= o->allocated;
        --g.remaining;
    }
done:
    if (g.lock >= 0) { (void)flock(g.lock, LOCK_UN); close(g.lock); }
    free(candidates);
    free(g.queue);
    free(g.index);
    free(g.objects);
    return rc;
}
