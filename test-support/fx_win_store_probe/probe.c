/* Independent native Win32 storage oracle; no Fortran/fpm build required. */
#include "../../include/fx_win_store.h"
#include <stdio.h>
#include <string.h>
#include <io.h>
#include <aclapi.h>
#include <stddef.h>

int fx_immutable_store_initialize(const char *);
int fx_immutable_store_validate(const char *);
int fx_immutable_lease_update(const char *, int, const char *, const char *,
    const char *, const char *, const char *, const char *, const char *,
    char *, size_t, long long *);
int fx_immutable_gc_collect(const char *, int, int, long long, long long,
    int, int *, long long *, int *, long long *);
void *fx_owned_begin_path(const char *, int);
int fx_owned_write(void *, const char *, int);
int fx_owned_fd(void *);
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

static int native_rename(const char *source, const char *destination)
{
    wchar_t *from = fx_win32_path(source), *to = fx_win32_path(destination);
    if (!from || !to) { free(from); free(to); return -1; }
    HANDLE handle = CreateFileW(from, DELETE | SYNCHRONIZE, FILE_SHARE_READ |
        FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_WRITE_THROUGH, NULL);
    free(from);
    if (handle == INVALID_HANDLE_VALUE) { free(to); return -1; }
    DWORD size = (DWORD)(offsetof(FILE_RENAME_INFO, FileName) + wcslen(to) * sizeof(*to));
    FILE_RENAME_INFO *info = calloc(1, size);
    if (!info) { free(to); CloseHandle(handle); return -1; }
    info->Flags = FILE_RENAME_FLAG_POSIX_SEMANTICS;
    info->FileNameLength = (DWORD)(wcslen(to) * sizeof(*to));
    memcpy(info->FileName, to, info->FileNameLength);
    int rc = SetFileInformationByHandle(handle, FileRenameInfoEx, info, size) ? 0 : -1;
    DWORD error = GetLastError(); free(to); free(info); CloseHandle(handle); SetLastError(error);
    return rc;
}

int fx_test_fs_lock(const char *);
int fx_test_fs_unlock(int);
int fx_test_fs_descriptor_count(void);
int fx_test_fs_lock_directory(const char *);
int fx_test_fs_temp_root(char *, int);
int fx_test_fs_mkdir_p(const char *);
int fx_test_fs_remove_tree(const char *);
int fx_test_fs_rename(const char *, const char *);
int fx_test_fs_chmod(const char *, int);
int fx_test_fs_sleep_ms(int64_t);
static int test_fs_oracles(const char *root)
{
    char temporary[4096], filename[4096], destination[4096], fixture[4096];
    int errors = 0, before, lock;
    wchar_t *saved = NULL;
    DWORD count = GetEnvironmentVariableW(L"TMPDIR", NULL, 0);
    if (count) {
        saved = malloc(count * sizeof(*saved));
        if (saved) GetEnvironmentVariableW(L"TMPDIR", saved, count);
    }
    path(temporary, root, "fs-temp");
    errors += check(fx_win_mkdirs_mode(temporary, 0700, 1) == 0, "native test temp root creates");
    wchar_t *wide = fx_win32_wide(temporary);
    errors += check(wide && SetEnvironmentVariableW(L"TMPDIR", wide), "test-only private TMPDIR selects");
    free(wide);
    errors += check(fx_test_fs_temp_root(temporary, sizeof(temporary)) == 0,
        "Fortran test support resolves actual native private temporary root");
    path(fixture, temporary, "nested-fixture");
    errors += check(fx_test_fs_mkdir_p(fixture) == 0, "native test-support mkdir works");
    path(filename, fixture, "lock");
    before = fx_test_fs_descriptor_count();
    lock = fx_test_fs_lock(filename);
    errors += check(before >= 0 && lock >= 0, "native test-support file lock owns");
    if (lock >= 0) errors += check(fx_test_fs_unlock(lock) == 0, "native test-support lock releases");
    errors += check(fx_test_fs_descriptor_count() == before, "independent native handle count detects no leak");
    lock = fx_test_fs_lock_directory(fixture);
    errors += check(lock >= 0, "native test-support directory lock owns");
    if (lock >= 0) errors += check(fx_test_fs_unlock(lock) == 0, "native directory ownership releases");
    errors += check(fx_test_fs_descriptor_count() == before, "directory mutex and descriptor both release");
    path(destination, fixture, "renamed-lock");
    errors += check(fx_test_fs_rename(filename, destination) == 0, "native test-support rename preserves fixture");
    errors += check(fx_test_fs_chmod(destination, 0444) == 0, "native test-support readonly chmod works");
    struct stat st;
    errors += check(lstat(destination, &st) == 0 && !(st.st_mode & 0222), "readonly test bytes are independently observed");
    ULONGLONG start = GetTickCount64();
    errors += check(fx_test_fs_sleep_ms(5) == 0 && GetTickCount64() - start >= 5,
        "native monotonic sleep honors its lower bound");
    errors += check(fx_test_fs_remove_tree(temporary) != 0, "destructive fixture API rejects its temp root");
    errors += check(fx_test_fs_remove_tree(fixture) == 0 && lstat(destination, &st) != 0 && errno == ENOENT,
        "native recursive cleanup removes only selected fixture and readonly bytes");
    SetEnvironmentVariableW(L"TMPDIR", saved); free(saved);
    return errors;
}

static int boundary_oracles(const char *root)
{
    char filename[4096], moved[4096], original[4096], alias[4096], outside[4096];
    char bytes[32] = {0}, stage[4096] = "", leaf[96] = "";
    struct stat st;
    int errors = 0, fd, parent, held;
    path(filename, root, "café-λ.f90");
    fd = open(filename, O_CREAT | O_RDWR | O_EXCL | O_NOFOLLOW, 0600);
    errors += check(fd >= 0 && _write(fd, "abcde", 5) == 5, "UTF-8 filename writes known bytes");
    if (fd >= 0) {
        errors += check(_lseeki64(fd, 2, SEEK_SET) == 2 && pread(fd, bytes, 1, 0) == 1 &&
            bytes[0] == 'a' && _lseeki64(fd, 0, SEEK_CUR) == 2,
            "pread preserves the original file position");
        errors += check(pwrite(fd, "Z", 1, 4) == 1 && _lseeki64(fd, 0, SEEK_CUR) == 2,
            "pwrite preserves the original file position");
        errors += check(_read(fd, bytes, 3) == 3 && !memcmp(bytes, "cdZ", 3),
            "positional write changes only the known byte");
        close(fd);
    }
    path(outside, root, "ordinary-outside");
    errors += check(fx_win_mkdirs_mode(outside, 0700, 1) == 0, "outside ordinary root exists");
    path(alias, root, "directory-alias");
    errors += check(symlink(outside, alias) == 0, "native directory reparse point creates");
    errors += check(lstat(alias, &st) == 0 && S_ISLNK(st.st_mode), "lstat identifies reparse alias");
    errors += check(fx_immutable_store_validate(alias) != 0,
        "immutable store rejects reparse root");
    char token[256]; long long epoch;
    errors += check(fx_immutable_lease_update(alias, 1, "owner", "start", "probe", "", "", "", "",
        token, sizeof(token), &epoch) == -2, "aliased root cannot register metadata");
    path(filename, outside, ".fx-metadata");
    errors += check(lstat(filename, &st) != 0 && errno == ENOENT,
        "reparse rejection leaves external ordinary root unchanged");
    errors += check(unlink(alias) == 0 && lstat(outside, &st) == 0 && S_ISDIR(st.st_mode),
        "unlinking directory alias preserves its external target");
    path(original, root, "held-original");
    path(moved, root, "held-moved");
    errors += check(fx_win_mkdirs_mode(original, 0700, 1) == 0, "held parent initializes");
    held = fx_win_open_directory(original);
    parent = fx_win_open_directory(root);
    errors += check(held >= 0 && parent >= 0 &&
        fx_win_rename_exclusive(parent, "held-original", parent, "held-moved") == 0,
        "held directory renames independently");
    errors += check(mkdir(original, 0700) == 0, "former parent name is replaced");
    fd = openat(held, "only-original", O_CREAT | O_RDWR | O_EXCL | O_NOFOLLOW, 0600);
    errors += check(fd >= 0, "relative creation uses the held parent object");
    if (fd >= 0) close(fd);
    path(filename, moved, "only-original");
    errors += check(lstat(filename, &st) == 0, "relative creation reaches original moved parent");
    path(filename, original, "only-original");
    errors += check(lstat(filename, &st) != 0 && errno == ENOENT,
        "replacement parent receives no writes");
    if (held >= 0) close(held);
    if (parent >= 0) close(parent);

    path(filename, root, "swap-target");
    /* Windows blocks renaming a directory containing an open child file.
     * A tree transaction lets the independent actor close its child first. */
    void *owned = fx_owned_begin_path(filename, 1);
    errors += check(owned != NULL, "swap oracle starts owned staging");
    if (owned) {
        fd = openat(fx_owned_fd(owned), "payload", O_CREAT | O_RDWR | O_EXCL, 0600);
        errors += check(fd >= 0 && _write(fd, "owned", 5) == 5,
            "owned tree payload writes before independent displacement");
        if (fd >= 0) close(fd);
        DIR *directory = opendir(root);
        struct dirent *entry;
        while (directory && (entry = readdir(directory))) {
            if (strncmp(entry->d_name, ".fx-owned-", 10)) continue;
            if (strlen(entry->d_name) >= sizeof(leaf)) {
                errors += check(0, "owned staging name exceeds oracle capacity"); break;
            }
            strcpy(leaf, entry->d_name); break;
        }
        if (directory) closedir(directory);
        errors += check(leaf[0] != 0, "independent directory oracle finds staging name");
        parent = fx_win_open_directory(root);
        path(stage, root, leaf);
        path(moved, root, "stolen-stage");
        int displaced = native_rename(stage, moved);
        printf("native-staging-rename rc=%d error=%lu\n", displaced, (unsigned long)GetLastError());
        errors += check(displaced == 0, "staging name is independently displaced");
        errors += check(mkdir(stage, 0700) == 0, "replacement staging directory creates");
        path(moved, stage, "payload");
        fd = open(moved, O_CREAT | O_RDWR | O_EXCL, 0600);
        errors += check(fd >= 0 && _write(fd, "rogue", 5) == 5, "replacement staging payload writes");
        if (fd >= 0) close(fd);
        errors += check(fx_owned_publish(owned) != 0 && lstat(filename, &st) != 0 && errno == ENOENT,
            "replaced staging cannot publish under an owned handle");
        path(moved, root, "stolen-stage/payload");
        fd = open(moved, O_RDONLY | O_NOFOLLOW);
        memset(bytes, 0, sizeof(bytes));
        errors += check(fd >= 0 && pread(fd, bytes, 5, 0) == 5 && !memcmp(bytes, "owned", 5),
            "displaced original bytes retain identity before owned cleanup");
        if (fd >= 0) close(fd);
        fx_owned_dispose(owned);
        path(moved, stage, "payload");
        fd = open(moved, O_RDONLY | O_NOFOLLOW);
        memset(bytes, 0, sizeof(bytes));
        errors += check(fd >= 0 && pread(fd, bytes, 5, 0) == 5 && !memcmp(bytes, "rogue", 5),
            "failed owner never deletes or changes replacement payload");
        if (fd >= 0) close(fd);
        if (parent >= 0) close(parent);
    }

    path(filename, root, "untrusted-acl");
    fd = open(filename, O_DIRECTORY | O_CREAT | O_EXCL | O_RDONLY, 0700);
    errors += check(fd >= 0, "private ACL fixture creates");
    if (fd >= 0) {
        unsigned char sid[SECURITY_MAX_SID_SIZE]; DWORD sid_size = sizeof(sid);
        PACL dacl = NULL;
        EXPLICIT_ACCESSW access = {0};
        errors += check(CreateWellKnownSid(WinWorldSid, NULL, sid, &sid_size), "Everyone SID resolves");
        access.grfAccessPermissions = FILE_ALL_ACCESS;
        access.grfAccessMode = GRANT_ACCESS;
        access.Trustee.TrusteeForm = TRUSTEE_IS_SID;
        access.Trustee.ptstrName = (wchar_t *)(void *)sid;
        DWORD result = SetEntriesInAclW(1, &access, NULL, &dacl);
        if (result == ERROR_SUCCESS) result = SetSecurityInfo((HANDLE)_get_osfhandle(fd),
            SE_FILE_OBJECT, DACL_SECURITY_INFORMATION | PROTECTED_DACL_SECURITY_INFORMATION,
            NULL, NULL, dacl, NULL);
        errors += check(result == ERROR_SUCCESS, "independent fixture grants untrusted access");
        errors += check(fx_win_private_owned(fd) == 0, "private guard rejects untrusted allow ACE");
        if (dacl) LocalFree(dacl);
        close(fd);
    }

    char long_directory[4096]; path(long_directory, root, "long-path");
    for (int i = 0; i < 3; ++i) {
        strcat(long_directory, "/");
        size_t end = strlen(long_directory);
        memset(long_directory + end, 'x' + i, 100); long_directory[end + 100] = 0;
    }
    errors += check(fx_win_mkdirs_mode(long_directory, 0700, 1) == 0,
        "native paths longer than MAX_PATH initialize");
    path(filename, long_directory, "café.f90");
    fd = open(filename, O_CREAT | O_RDWR | O_EXCL | O_NOFOLLOW, 0600);
    errors += check(fd >= 0 && _write(fd, "long", 4) == 4, "long UTF-8 path writes");
    if (fd >= 0) close(fd);
    errors += check(stat(filename, &st) == 0 && st.st_size == 4, "stat preserves long UTF-8 paths");
    char *canonical = realpath(filename, NULL);
    errors += check(canonical && strlen(canonical) > 260 && strstr(canonical, "café.f90"),
        "allocated realpath resolves complete long UTF-8 filename");
    free(canonical);
    return errors;
}

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
    errors += boundary_oracles(root);
    errors += test_fs_oracles(root);
    printf("native-store-errors=%d\n", errors);
    return errors ? 1 : 0;
}
