/* Independent native Win32 storage oracle; no Fortran/fpm build required. */
#include "../../include/fx_win_store.h"
#include <stdio.h>
#include <string.h>
#include <io.h>

int fx_immutable_store_initialize(const char *);
int fx_immutable_store_validate(const char *);
int fx_immutable_lease_update(const char *, int, const char *, const char *,
    const char *, const char *, const char *, const char *, const char *,
    char *, size_t, long long *);
int fx_immutable_gc_collect(const char *, int, int, long long, long long,
    int, int *, long long *, int *, long long *);
void *fx_owned_begin_path(const char *, int);
int fx_owned_write(void *, const char *, int);
int fx_owned_publish(void *);
void fx_owned_dispose(void *);
int fx_action_result_lock(const char *, const char *);
int fx_action_result_unlock(int);
int fx_action_result_write(const char *, const char *, const char *, int);
int fx_action_result_read_touch(const char *, const char *, char *, int, int *);

static int check(int condition, const char *claim)
{
    if (condition) return 0;
    fprintf(stderr, "FAIL %s errno=%d winerror=%lu\n", claim, errno,
        (unsigned long)GetLastError()); return 1;
}
static void path(char *out, const char *root, const char *name)
{ snprintf(out, 4096, "%s/%s", root, name); }

int main(int argc, char **argv)
{
    const char *id = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824";
    char root[4096], target[4096], metadata[4096], token[256], bytes[256];
    int errors = 0, fd, rc, scanned, deleted, count;
    long long epoch, allocated, reclaimed;
    struct stat st;
    if (argc == 3 && !strcmp(argv[1], "--lock-child")) {
        fd = open(argv[2], O_RDWR | O_NOFOLLOW);
        errors += check(fd >= 0, "child opens owner record");
        if (fd < 0) return 1;
        rc = flock(fd, LOCK_EX | LOCK_NB);
        errors += check(rc != 0 && errno == EWOULDBLOCK, "child sees live ownership");
        memset(bytes, 0, sizeof(bytes));
        errors += check(pread(fd, bytes, 5, 0) == 5 && !memcmp(bytes, "owner", 5),
            "advisory lock permits owner-record reads");
        close(fd); return errors ? 1 : 0;
    }
    if (argc != 2) return 2;
    strcpy(root, argv[1]);
    errors += check(fx_win_mkdirs_mode(root, 0700, 1) == 0, "private probe root initializes");
    fd = fx_win_open_directory(root);
    errors += check(fd >= 0 && fx_win_private_owned(fd) == 1, "real owner SID and protected DACL");
    if (fd >= 0) close(fd);
    path(metadata, root, ".fx-metadata");
    rc = fx_immutable_lease_update(root, 1, "owner", "start", "probe", "", "", "", "",
        token, sizeof(token), &epoch);
    errors += check(rc == -2 && lstat(metadata, &st) != 0 && errno == ENOENT,
        "ordinary root refuses leases without metadata writes");
    rc = fx_immutable_gc_collect(root, 100, 10, 0, 0, 0, &scanned, &allocated,
        &deleted, &reclaimed);
    errors += check(rc == -2 && lstat(metadata, &st) != 0 && errno == ENOENT,
        "ordinary root refuses GC without writes");
    errors += check(fx_immutable_store_initialize(root) == 0 &&
        fx_immutable_store_validate(root) == 0, "canonical namespaces initialize");
    path(target, root, "blobs/sha256/2c");
    errors += check(fx_win_mkdirs(target, 1) == 0, "blob shard initializes durably");
    path(target, root, "blobs/sha256/2c/2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824");
    void *owned = fx_owned_begin_path(target, 0);
    errors += check(owned != NULL, "owned native staging begins");
    if (owned) {
        errors += check(fx_owned_write(owned, "hello", 5) == 0, "owned bytes write");
        errors += check(fx_owned_publish(owned) == 0, "exclusive held-file publication");
        fx_owned_dispose(owned);
    }
    fd = open(target, O_RDONLY | O_NOFOLLOW);
    memset(bytes, 0, sizeof(bytes));
    errors += check(fd >= 0 && pread(fd, bytes, 5, 0) == 5 && !memcmp(bytes, "hello", 5),
        "published native object retains known bytes");
    if (fd >= 0) close(fd);
    owned = fx_owned_begin_path(target, 0);
    if (owned) {
        fx_owned_write(owned, "other", 5);
        errors += check(fx_owned_publish(owned) == 1, "exclusive collision preserves original");
        fx_owned_dispose(owned);
    } else errors += check(0, "duplicate staging begins");
    if (lstat(target, &st) == 0) printf("published-mode=%o nlink=%llu size=%lld\n",
        st.st_mode, (unsigned long long)st.st_nlink, (long long)st.st_size);
    rc = fx_immutable_lease_update(root, 3, "owner", "start", "probe", "", "blob", id, "",
        token, sizeof(token), &epoch);
    errors += check(rc == 0, "native active pin commits");
    rc = fx_immutable_gc_collect(root, 100, 10, 0, 0, 0, &scanned, &allocated,
        &deleted, &reclaimed);
    printf("pinned-gc rc=%d scanned=%d deleted=%d allocated=%lld reclaimed=%lld\n",
        rc, scanned, deleted, allocated, reclaimed);
    errors += check(rc == 0 && deleted == 0 && lstat(target, &st) == 0,
        "GC preserves actively pinned immutable bytes");
    rc = fx_immutable_lease_update(root, 4, "owner", "start", "probe", token, "", "", "",
        NULL, 0, &epoch);
    errors += check(rc == 0, "pin release commits");
    rc = fx_immutable_gc_collect(root, 100, 10, 0, 0, 0, &scanned, &allocated,
        &deleted, &reclaimed);
    printf("released-gc rc=%d scanned=%d deleted=%d allocated=%lld reclaimed=%lld\n",
        rc, scanned, deleted, allocated, reclaimed);
    errors += check(rc == 0 && deleted == 1 && lstat(target, &st) != 0 && errno == ENOENT,
        "GC deletes only the released eligible object");
    int lock = fx_action_result_lock(root, id);
    errors += check(lock >= 0, "action owner lock opens");
    errors += check(fx_action_result_write(root, id, "known-record", 12) == 0,
        "action record atomically writes");
    memset(bytes, 0, sizeof(bytes));
    rc = fx_action_result_read_touch(root, id, bytes, sizeof(bytes), &count);
    errors += check(rc == 0 && count == 12 && !memcmp(bytes, "known-record", 12),
        "touched action record retains complete bytes");
    if (lock >= 0) errors += check(fx_action_result_unlock(lock) == 0, "action lock releases");
    path(target, root, "owner-record");
    fd = open(target, O_CREAT | O_RDWR | O_EXCL | O_NOFOLLOW, 0600);
    errors += check(fd >= 0 && _write(fd, "owner", 5) == 5, "native owner marker writes");
    errors += check(flock(fd, LOCK_EX) == 0 && flock(fd, LOCK_EX) == 0,
        "same owner relock is idempotent");
    wchar_t exe[4096], command[12288];
    GetModuleFileNameW(NULL, exe, 4096);
    wchar_t *wide = fx_win32_wide(target);
    swprintf(command, 12288, L"\"%ls\" --lock-child \"%ls\"", exe, wide);
    free(wide);
    STARTUPINFOW startup = {0}; PROCESS_INFORMATION child;
    startup.cb = sizeof(startup);
    if (CreateProcessW(exe, command, NULL, NULL, FALSE, 0, NULL, NULL, &startup, &child)) {
        DWORD code;
        WaitForSingleObject(child.hProcess, INFINITE);
        GetExitCodeProcess(child.hProcess, &code);
        errors += check(code == 0, "independent child lock/read oracle passes");
        CloseHandle(child.hProcess); CloseHandle(child.hThread);
    } else errors += check(0, "independent child starts");
    errors += check(flock(fd, LOCK_UN) == 0 && flock(fd, LOCK_EX | LOCK_NB) == 0,
        "one unlock releases idempotent ownership");
    close(fd);
    printf("native-store-errors=%d\n", errors);
    return errors ? 1 : 0;
}
