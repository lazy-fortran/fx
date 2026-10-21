/* Size-bounded eviction for the immutable action store.
 *
 * Shared caches live on filesystems whose locks are host-local (BeeGFS) and
 * whose rename-replace is not atomic for readers (BeeGFS ENOENT, NFSv3
 * ESTALE). Eviction therefore takes no lock. Safety rests on three facts:
 * objects are content-addressed and every reader verifies their hash, so a
 * deleted or replaced file can only cause a miss; publishers freshen the
 * mtime of every object they reuse and readers refresh the record's mtime
 * (its last-use stamp) before using it; and nothing whose mtime lies within
 * the grace period is retired or deleted. Concurrent evictors on several
 * hosts may each delete; they only ever remove unreachable or least recently
 * used entries.
 *
 * Policy: when allocated object bytes exceed max_bytes, retire the least
 * recently used action records (all older than grace) until the objects
 * still reachable, plus all objects younger than grace, fit in low_bytes;
 * then delete unreachable objects older than grace. Staging leftovers older
 * than temp_age are removed on every run. Lease rows of other owners
 * (Gremlin generations) are roots; rows written by older action-store
 * versions are ignored, as action records now are the roots.
 */
#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#else
#define _POSIX_C_SOURCE 200809L
#endif
#include <errno.h>
#include <stdlib.h>

int fx_immutable_gc_supported(const char *root);

#ifdef _WIN32
int fx_store_evict(const char *root, long long max_bytes, long long low_bytes,
                   long long grace, long long temp_age, long long *stats)
{
    (void)root; (void)max_bytes; (void)low_bytes; (void)grace;
    (void)temp_age; (void)stats;
    return -2;
}
#else
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
#define RECORD_LIMIT 512
#define TREE_LIMIT (8 * 1024 * 1024)
#define LEASE_LIMIT (64 * 1024 * 1024)

typedef struct {
    char id[65];
    char kind; /* 'B' or 'T' */
    long long allocated;
    time_t mtime;
    dev_t dev;
    ino_t ino;
    off_t size;
    int mark, young, pinned;
    size_t child_off, nchild;
} obj_t;

typedef struct {
    char id[65];
    time_t mtime;
    dev_t dev;
    ino_t ino;
    long trees[2];
    int ntrees, retired;
} act_t;

typedef struct {
    const char *root;
    time_t now;
    long long grace, temp_age;
    obj_t *objs;
    size_t nobj, capobj;
    act_t *acts;
    size_t nact, capact;
    long *children;
    size_t nchildren, capchildren;
    long *stack;
    long long total, temps;
} ev_t;

static int hexid(const char *s)
{
    if (strlen(s) != 64) return 0;
    for (int i = 0; i < 64; ++i)
        if (!((s[i] >= '0' && s[i] <= '9') || (s[i] >= 'a' && s[i] <= 'f')))
            return 0;
    return 1;
}

static int grow(void **p, size_t *cap, size_t need, size_t size)
{
    if (need <= *cap) return 0;
    size_t n = *cap ? *cap * 2 : 1024;
    while (n < need) n *= 2;
    void *q = realloc(*p, n * size);
    if (!q) return -1;
    *p = q;
    *cap = n;
    return 0;
}

static int remove_tree_at(int dir, const char *name)
{
    struct stat st;
    if (fstatat(dir, name, &st, AT_SYMLINK_NOFOLLOW) != 0)
        return errno == ENOENT ? 0 : -1;
    if (!S_ISDIR(st.st_mode))
        return unlinkat(dir, name, 0) == 0 || errno == ENOENT ? 0 : -1;
    int fd = openat(dir, name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) return errno == ENOENT ? 0 : -1;
    DIR *d = fdopendir(fd);
    if (!d) { close(fd); return -1; }
    struct dirent *e;
    while ((e = readdir(d)) != NULL) {
        if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
        (void)remove_tree_at(fd, e->d_name);
    }
    closedir(d);
    return unlinkat(dir, name, AT_REMOVEDIR) == 0 || errno == ENOENT ? 0 : -1;
}

/* Staging leftovers of crashed writers: removed once older than temp_age. */
static void reap_temp(ev_t *g, int dir, const char *name)
{
    struct stat st;
    if (fstatat(dir, name, &st, AT_SYMLINK_NOFOLLOW) != 0) return;
    if (st.st_mtime > g->now || (long long)(g->now - st.st_mtime) < g->temp_age)
        return;
    if (remove_tree_at(dir, name) == 0) ++g->temps;
}

static int is_temp(const char *name)
{
    return !strncmp(name, ".fx-owned-", 10) || !strncmp(name, ".fx-copy-", 9) ||
           !strncmp(name, ".tmp.", 5);
}

static int open_sub(const char *root, const char *a, const char *b)
{
    char path[PATH_MAX];
    int n = snprintf(path, sizeof(path), "%s/%s/%s", root, a, b);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    return open(path, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
}

static int scan_objects(ev_t *g, const char *kinddir, char kind)
{
    int top = open_sub(g->root, kinddir, "sha256");
    if (top < 0) return errno == ENOENT ? 0 : -1;
    DIR *d = fdopendir(top);
    if (!d) { close(top); return -1; }
    struct dirent *s;
    while ((s = readdir(d)) != NULL) {
        if (strlen(s->d_name) != 2 || s->d_name[0] == '.') continue;
        int sd = openat(top, s->d_name, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
        if (sd < 0) continue;
        DIR *sh = fdopendir(sd);
        if (!sh) { close(sd); continue; }
        struct dirent *e;
        while ((e = readdir(sh)) != NULL) {
            if (e->d_name[0] == '.') {
                if (is_temp(e->d_name)) reap_temp(g, sd, e->d_name);
                continue;
            }
            struct stat st;
            if (!hexid(e->d_name) ||
                fstatat(sd, e->d_name, &st, AT_SYMLINK_NOFOLLOW) != 0 ||
                !S_ISREG(st.st_mode)) continue;
            if (grow((void **)&g->objs, &g->capobj, g->nobj + 1,
                     sizeof(obj_t)) != 0) { closedir(sh); closedir(d); return -1; }
            obj_t *o = &g->objs[g->nobj++];
            memset(o, 0, sizeof(*o));
            memcpy(o->id, e->d_name, 65);
            o->kind = kind;
            o->allocated = (long long)st.st_blocks * 512;
            o->mtime = st.st_mtime;
            o->dev = st.st_dev;
            o->ino = st.st_ino;
            o->size = st.st_size;
            o->young = st.st_mtime > g->now - g->grace;
            g->total += o->allocated;
        }
        closedir(sh);
    }
    closedir(d);
    return 0;
}

static int cmp_obj(const void *l, const void *r)
{
    const obj_t *a = l, *b = r;
    if (a->kind != b->kind) return a->kind < b->kind ? -1 : 1;
    return strcmp(a->id, b->id);
}

static long find_obj(ev_t *g, char kind, const char *id)
{
    size_t lo = 0, hi = g->nobj;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        obj_t *o = &g->objs[mid];
        int c = o->kind == kind ? strcmp(o->id, id) : (o->kind < kind ? -1 : 1);
        if (c < 0) lo = mid + 1;
        else hi = mid;
    }
    if (lo < g->nobj && g->objs[lo].kind == kind && !strcmp(g->objs[lo].id, id))
        return (long)lo;
    return -1;
}

static int read_at(int dir, const char *name, size_t limit, char **out,
                   size_t *size)
{
    int fd = openat(dir, name, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return -1;
    struct stat st;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode) || st.st_size < 0 ||
        (size_t)st.st_size > limit) { close(fd); return -1; }
    char *buf = malloc((size_t)st.st_size + 1);
    if (!buf) { close(fd); return -1; }
    size_t got = 0;
    while (got < (size_t)st.st_size) {
        ssize_t n = read(fd, buf + got, (size_t)st.st_size - got);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) { free(buf); close(fd); return -1; }
        got += (size_t)n;
    }
    close(fd);
    buf[got] = '\0';
    *out = buf;
    *size = got;
    return 0;
}

/* Parse every tree once; unreadable trees simply have no children. */
static int load_children(ev_t *g)
{
    int top = open_sub(g->root, "trees", "sha256");
    if (top < 0) return errno == ENOENT ? 0 : -1;
    for (size_t i = 0; i < g->nobj; ++i) {
        obj_t *o = &g->objs[i];
        if (o->kind != 'T') continue;
        char shard[3] = {o->id[0], o->id[1], '\0'};
        int sd = openat(top, shard, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
        if (sd < 0) continue;
        char *data = NULL, *save = NULL;
        size_t size;
        int rc = read_at(sd, o->id, TREE_LIMIT, &data, &size);
        close(sd);
        if (rc != 0) continue;
        o->child_off = g->nchildren;
        if (size >= 8 && !memcmp(data, "FXTREE1\n", 8)) {
            for (char *line = strtok_r(data + 8, "\n", &save); line;
                 line = strtok_r(NULL, "\n", &save)) {
                char *f[5];
                int nf = 0;
                f[nf++] = line;
                for (char *c = line; *c && nf <= 5; ++c)
                    if (*c == '\t') { *c = '\0'; if (nf < 5) f[nf] = c + 1; ++nf; }
                if (nf != 5 || (f[0][0] != 'B' && f[0][0] != 'T')) continue;
                long child = find_obj(g, f[0][0], f[4]);
                if (child < 0) continue;
                if (grow((void **)&g->children, &g->capchildren,
                         g->nchildren + 1, sizeof(long)) != 0) {
                    free(data); close(top); return -1;
                }
                g->children[g->nchildren++] = child;
            }
        }
        o->nchild = g->nchildren - o->child_off;
        free(data);
    }
    close(top);
    return 0;
}

static void mark_from(ev_t *g, long start)
{
    size_t depth = 0;
    if (start < 0 || g->objs[start].mark) return;
    g->objs[start].mark = 1;
    g->stack[depth++] = start;
    while (depth) {
        obj_t *o = &g->objs[g->stack[--depth]];
        for (size_t c = 0; c < o->nchild; ++c) {
            long k = g->children[o->child_off + c];
            if (g->objs[k].mark) continue;
            g->objs[k].mark = 1;
            g->stack[depth++] = k;
        }
    }
}

/* FXACTION2 records: BOUND names one tree, a conflict names two, a legacy
 * RETIRED marker none. Returns the number of trees or -1 when malformed. */
static int parse_record(char *data, const char *id, char ids[2][65])
{
    char *save = NULL;
    char *line = strtok_r(data, "\n", &save);
    if (!line || strcmp(line, "FXACTION2")) return -1;
    line = strtok_r(NULL, "\n", &save);
    if (!line || strcmp(line, id)) return -1;
    line = strtok_r(NULL, "\n", &save);
    if (!line) return -1;
    if (!strcmp(line, "RETIRED")) return 0;
    int want = !strcmp(line, "BOUND") ? 1 :
               !strcmp(line, "NONDETERMINISTIC_ACTION incomplete-key") ? 2 : 0;
    if (!want) return -1;
    for (int k = 0; k < want; ++k) {
        line = strtok_r(NULL, "\n", &save);
        if (!line || !hexid(line)) return -1;
        strcpy(ids[k], line);
    }
    return strtok_r(NULL, "\n", &save) ? -1 : want;
}

static int scan_actions(ev_t *g, char (**tree_ids)[2][65])
{
    char base[PATH_MAX];
    int n = snprintf(base, sizeof(base), "%s/actions/sha256", g->root);
    if (n < 0 || (size_t)n >= sizeof(base)) return -1;
    int top = open(base, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
    if (top < 0) return errno == ENOENT ? 0 : -1;
    DIR *d = fdopendir(top);
    if (!d) { close(top); return -1; }
    size_t capids = 0;
    struct dirent *s;
    while ((s = readdir(d)) != NULL) {
        if (strlen(s->d_name) != 2 || s->d_name[0] == '.') continue;
        int sd = openat(top, s->d_name, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
        if (sd < 0) continue;
        DIR *sh = fdopendir(sd);
        if (!sh) { close(sd); continue; }
        struct dirent *e;
        while ((e = readdir(sh)) != NULL) {
            size_t len = strlen(e->d_name);
            if (e->d_name[0] == '.') {
                if (is_temp(e->d_name)) reap_temp(g, sd, e->d_name);
                continue;
            }
            /* Advisory per-action lock files carry no state. */
            if (len == 69 && !strcmp(e->d_name + 64, ".lock")) {
                reap_temp(g, sd, e->d_name);
                continue;
            }
            if (!hexid(e->d_name)) continue;
            char *data = NULL;
            size_t size;
            struct stat st;
            if (fstatat(sd, e->d_name, &st, AT_SYMLINK_NOFOLLOW) != 0 ||
                !S_ISREG(st.st_mode)) continue;
            if (read_at(sd, e->d_name, RECORD_LIMIT, &data, &size) != 0)
                continue;
            if (grow((void **)&g->acts, &g->capact, g->nact + 1,
                     sizeof(act_t)) != 0 ||
                grow((void **)tree_ids, &capids, g->nact + 1,
                     sizeof(**tree_ids)) != 0) {
                free(data); closedir(sh); closedir(d); return -1;
            }
            act_t *a = &g->acts[g->nact];
            memset(a, 0, sizeof(*a));
            memcpy(a->id, e->d_name, 65);
            a->mtime = st.st_mtime;
            a->dev = st.st_dev;
            a->ino = st.st_ino;
            /* -1: unparseable, retired once older than grace. */
            a->ntrees = parse_record(data, a->id, (*tree_ids)[g->nact]);
            (void)size;
            free(data);
            ++g->nact;
        }
        closedir(sh);
    }
    closedir(d);
    return 0;
}

static int cmp_act(const void *l, const void *r)
{
    const act_t *a = l, *b = r;
    if (a->mtime != b->mtime) return a->mtime < b->mtime ? -1 : 1;
    return strcmp(a->id, b->id);
}

/* Lease rows of other owners pin objects. Rows from older action-store
 * versions (start "fx-action-v1", owners fx-publisher/fx-action-files) are
 * superseded by records and the grace period. Return -1 when the snapshot
 * exists but cannot be read: then nothing may be deleted. */
static int pin_leases(ev_t *g)
{
    char path[PATH_MAX];
    int n = snprintf(path, sizeof(path), "%s/.fx-metadata", g->root);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    int meta = open(path, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
    if (meta < 0) return errno == ENOENT ? 0 : -1;
    char *data = NULL, *save = NULL;
    size_t size;
    int rc = read_at(meta, "leases", LEASE_LIMIT, &data, &size);
    int saved = errno;
    close(meta);
    if (rc != 0) return saved == ENOENT ? 0 : -1;
    int group_skip = 1;
    char group_kind = 0;
    for (char *line = strtok_r(data, "\n", &save); line;
         line = strtok_r(NULL, "\n", &save)) {
        char *f[8];
        int nf = 0;
        f[nf++] = line;
        for (char *c = line; *c && nf <= 8; ++c)
            if (*c == '|') { *c = '\0'; if (nf < 8) f[nf] = c + 1; ++nf; }
        if (nf == 2 && !strcmp(f[0], "O")) {
            if (!group_skip && group_kind) {
                long o = find_obj(g, group_kind, f[1]);
                if (o >= 0) g->objs[o].pinned = 1;
            }
            continue;
        }
        if (nf == 7 && !strcmp(f[0], "G")) {
            /* G|type|token|owner|start|reason|kind */
            group_skip = !strcmp(f[4], "fx-action-v1") ||
                         !strcmp(f[3], "fx-publisher") ||
                         !strcmp(f[3], "fx-action-files");
            group_kind = !strcmp(f[6], "blob") ? 'B' :
                         !strcmp(f[6], "tree") ? 'T' : 0;
            continue;
        }
        if (nf != 7) continue;
        /* type|token|owner|start|reason|kind|id */
        if (!strcmp(f[3], "fx-action-v1") || !strcmp(f[2], "fx-publisher") ||
            !strcmp(f[2], "fx-action-files")) continue;
        char kind = !strcmp(f[5], "blob") ? 'B' : !strcmp(f[5], "tree") ? 'T' : 0;
        if (!kind || !hexid(f[6])) continue;
        long o = find_obj(g, kind, f[6]);
        if (o >= 0) g->objs[o].pinned = 1;
    }
    free(data);
    return 0;
}

static long long retained_bytes(ev_t *g, size_t first_live)
{
    long long sum = 0;
    for (size_t i = 0; i < g->nobj; ++i) g->objs[i].mark = 0;
    for (size_t i = 0; i < g->nobj; ++i)
        if (g->objs[i].pinned) mark_from(g, (long)i);
    for (size_t i = first_live; i < g->nact; ++i) {
        act_t *a = &g->acts[i];
        if (a->retired) continue;
        for (int t = 0; t < a->ntrees; ++t) mark_from(g, a->trees[t]);
    }
    for (size_t i = 0; i < g->nobj; ++i)
        if (g->objs[i].mark || g->objs[i].young) sum += g->objs[i].allocated;
    return sum;
}

static int unlink_if_same(const char *root, const char *sub, const char *id,
                          dev_t dev, ino_t ino, time_t mtime)
{
    char path[PATH_MAX];
    int n = snprintf(path, sizeof(path), "%s/%s/sha256/%.2s/%s", root, sub, id, id);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    struct stat st;
    /* A freshened or replaced entry is in use again: keep it. */
    if (lstat(path, &st) != 0 || st.st_dev != dev || st.st_ino != ino ||
        st.st_mtime != mtime) return -1;
    return unlink(path) == 0 ? 0 : -1;
}

/* stats: [0] bytes before, [1] bytes after, [2] actions, [3] actions retired,
 * [4] objects deleted, [5] temps removed, [6] 1 when over the cap after. */
int fx_store_evict(const char *root, long long max_bytes, long long low_bytes,
                   long long grace, long long temp_age, long long *stats)
{
    ev_t g;
    char (*tree_ids)[2][65] = NULL;
    int rc = -1;
    memset(&g, 0, sizeof(g));
    for (int i = 0; i < 7; ++i) stats[i] = 0;
    if (!root || max_bytes < 0 || low_bytes < 0 || low_bytes > max_bytes ||
        grace < 0 || temp_age < 0) return -2;
    int supported = fx_immutable_gc_supported(root);
    if (supported <= 0) return supported == 0 ? -3 : -1;
    g.root = root;
    g.now = time(NULL);
    g.grace = grace;
    g.temp_age = temp_age;
    if (g.now == (time_t)-1) return -1;
    if (scan_objects(&g, "blobs", 'B') != 0 ||
        scan_objects(&g, "trees", 'T') != 0) goto done;
    stats[0] = stats[1] = g.total;
    if (scan_actions(&g, &tree_ids) != 0) goto done;
    stats[2] = (long long)g.nact;
    if (g.total <= max_bytes) { rc = 0; goto done; }

    qsort(g.objs, g.nobj, sizeof(obj_t), cmp_obj);
    for (size_t i = 0; i < g.nact; ++i) {
        act_t *a = &g.acts[i];
        int k = 0;
        for (int t = 0; t < a->ntrees; ++t) {
            long o = find_obj(&g, 'T', tree_ids[i][t]);
            if (o >= 0) a->trees[k++] = o;
        }
        if (a->ntrees >= 0) a->ntrees = k;
    }
    /* Sorting actions detaches tree_ids, which are resolved already. */
    qsort(g.acts, g.nact, sizeof(act_t), cmp_act);
    g.stack = malloc((g.nobj ? g.nobj : 1) * sizeof(long));
    if (!g.stack || load_children(&g) != 0) goto done;
    if (pin_leases(&g) != 0) goto done;

    size_t eligible = 0;
    while (eligible < g.nact && g.acts[eligible].mtime <= g.now - grace)
        ++eligible;
    size_t lo = 0, hi = eligible;
    if (retained_bytes(&g, eligible) > low_bytes) lo = hi = eligible;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (retained_bytes(&g, mid) <= low_bytes) hi = mid;
        else lo = mid + 1;
    }
    char apath[PATH_MAX];
    for (size_t i = 0; i < lo; ++i) {
        act_t *a = &g.acts[i];
        int n = snprintf(apath, sizeof(apath), "%s/actions/sha256/%.2s/%s",
                         root, a->id, a->id);
        struct stat st;
        if (n < 0 || (size_t)n >= sizeof(apath)) continue;
        /* A record read (touched) or rewritten since the scan stays. */
        if (lstat(apath, &st) != 0 || st.st_ino != a->ino ||
            st.st_dev != a->dev || st.st_mtime != a->mtime) continue;
        if (unlink(apath) == 0) { a->retired = 1; ++stats[3]; }
    }
    /* Unparseable records older than grace are garbage too. */
    for (size_t i = lo; i < eligible; ++i) {
        act_t *a = &g.acts[i];
        if (a->ntrees >= 0) continue;
        int n = snprintf(apath, sizeof(apath), "%s/actions/sha256/%.2s/%s",
                         root, a->id, a->id);
        if (n > 0 && (size_t)n < sizeof(apath) && unlink(apath) == 0) {
            a->retired = 1;
            ++stats[3];
        }
    }
    (void)retained_bytes(&g, 0);
    for (size_t i = 0; i < g.nobj; ++i) {
        obj_t *o = &g.objs[i];
        if (o->mark || o->young || o->pinned) continue;
        if (unlink_if_same(root, o->kind == 'B' ? "blobs" : "trees", o->id,
                           o->dev, o->ino, o->mtime) == 0) {
            ++stats[4];
            g.total -= o->allocated;
        }
    }
    stats[1] = g.total;
    stats[6] = g.total > max_bytes;
    rc = 0;
done:
    stats[5] = g.temps;
    free(tree_ids);
    free(g.objs);
    free(g.acts);
    free(g.children);
    free(g.stack);
    return rc;
}
#endif

#ifndef _WIN32
/* Claim the periodic eviction run: 1 when the stamp is missing or older than
 * interval (and is refreshed), 0 otherwise. Several hosts may both claim a
 * run; concurrent evictors are safe, the stamp only limits the scan rate. */
int fx_store_evict_due(const char *root, long long interval)
{
    int supported = fx_immutable_gc_supported(root);
    if (supported <= 0) return supported == 0 ? 0 : -1;
    char path[PATH_MAX];
    int n = snprintf(path, sizeof(path), "%s/.fx-metadata/evict-stamp", root);
    if (n < 0 || (size_t)n >= sizeof(path)) return -1;
    struct stat st;
    time_t now = time(NULL);
    if (stat(path, &st) == 0 && st.st_mtime <= now &&
        (long long)(now - st.st_mtime) < interval) return 0;
    int fd = open(path, O_WRONLY | O_CREAT | O_CLOEXEC | O_NOFOLLOW, 0644);
    if (fd < 0) {
        char dir[PATH_MAX];
        n = snprintf(dir, sizeof(dir), "%s/.fx-metadata", root);
        if (n < 0 || (size_t)n >= sizeof(dir) ||
            (mkdir(dir, 0755) != 0 && errno != EEXIST)) return -1;
        fd = open(path, O_WRONLY | O_CREAT | O_CLOEXEC | O_NOFOLLOW, 0644);
        if (fd < 0) return -1;
    }
    int rc = futimens(fd, NULL);
    close(fd);
    return rc == 0 ? 1 : -1;
}
#else
int fx_store_evict_due(const char *root, long long interval)
{
    (void)root; (void)interval;
    return 0;
}
#endif
