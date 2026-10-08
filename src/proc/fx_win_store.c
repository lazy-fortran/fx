/* Win32 storage primitives. The Fortran/store algorithms remain shared. */
#ifdef _WIN32
#define FX_WIN_STORE_IMPLEMENTATION
#include "../../include/fx_win_store.h"
#include <winternl.h>
#include <aclapi.h>
#include <sddl.h>
#include <winioctl.h>
#include <io.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <unistd.h>

#define FX_NT_DIRECTORY 0x00000001UL
#define FX_NT_WRITE_THROUGH 0x00000002UL
#define FX_NT_SYNCHRONOUS 0x00000020UL
#define FX_NT_REPARSE 0x00200000UL
#define FX_NT_OPEN 1UL
#define FX_NT_CREATE 2UL
#define FX_NT_OPEN_IF 3UL
#define FX_NT_OVERWRITE_IF 5UL

typedef NTSTATUS (NTAPI *create_fn)(PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES,
    PIO_STATUS_BLOCK, PLARGE_INTEGER, ULONG, ULONG, ULONG, ULONG, PVOID, ULONG);
typedef NTSTATUS (NTAPI *set_fn)(HANDLE, PIO_STATUS_BLOCK, PVOID, ULONG,
    FILE_INFORMATION_CLASS);
typedef ULONG (WINAPI *error_fn)(NTSTATUS);

static int nt_error(NTSTATUS status)
{
    error_fn convert = (error_fn)(void *)GetProcAddress(
        GetModuleHandleW(L"ntdll.dll"), "RtlNtStatusToDosError");
    return fx_win32_errno(convert ? convert(status) : ERROR_GEN_FAILURE);
}

static HANDLE handle_of(int fd)
{
    intptr_t raw = _get_osfhandle(fd);
    return raw == -1 ? INVALID_HANDLE_VALUE : (HANDLE)raw;
}

static int descriptor(HANDLE handle, int flags)
{
    int fd = _open_osfhandle((intptr_t)handle,
        _O_BINARY | _O_NOINHERIT | (flags & O_ACCMODE));
    if (fd < 0) CloseHandle(handle);
    return fd;
}

static TOKEN_USER *current_user(void)
{
    HANDLE token;
    DWORD count = 0;
    TOKEN_USER *user;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) return NULL;
    GetTokenInformation(token, TokenUser, NULL, 0, &count);
    user = malloc(count);
    if (!user) { CloseHandle(token); SetLastError(ERROR_NOT_ENOUGH_MEMORY); return NULL; }
    if (!GetTokenInformation(token, TokenUser, user, count, &count)) {
        DWORD error = GetLastError(); free(user); CloseHandle(token);
        SetLastError(error); return NULL;
    }
    CloseHandle(token); return user;
}
static PSECURITY_DESCRIPTOR private_descriptor(void)
{
    TOKEN_USER *user = current_user();
    LPWSTR sid = NULL;
    PSECURITY_DESCRIPTOR descriptor = NULL;
    wchar_t *sddl;
    size_t count;
    if (!user) return NULL;
    if (!ConvertSidToStringSidW(user->User.Sid, &sid)) { free(user); return NULL; }
    count = 2 * wcslen(sid) + 32;
    sddl = malloc(count * sizeof(*sddl));
    if (!sddl) { LocalFree(sid); free(user); SetLastError(ERROR_NOT_ENOUGH_MEMORY); return NULL; }
    swprintf(sddl, count, L"O:%lsD:P(A;;FA;;;%ls)", sid, sid);
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(sddl,
        SDDL_REVISION_1, &descriptor, NULL)) descriptor = NULL;
    DWORD error = GetLastError(); free(sddl); LocalFree(sid); free(user); SetLastError(error);
    return descriptor;
}

static HANDLE child_handle(HANDLE parent, const wchar_t *name, int flags,
                           ACCESS_MASK extra, int allow_reparse, int mode)
{
    UNICODE_STRING text;
    OBJECT_ATTRIBUTES object;
    IO_STATUS_BLOCK io;
    HANDLE handle = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    DWORD attributes;
    ULONG disposition = FX_NT_OPEN;
    ACCESS_MASK access = FILE_READ_ATTRIBUTES | READ_CONTROL | SYNCHRONIZE | extra;
    PSECURITY_DESCRIPTOR security = NULL;
    create_fn create = (create_fn)(void *)GetProcAddress(
        GetModuleHandleW(L"ntdll.dll"), "NtCreateFile");
    size_t length = wcslen(name);
    if (!create || !length || length > 32766 || wcschr(name, L'\\') ||
        wcschr(name, L'/') || wcschr(name, L':') || !wcscmp(name, L"..")) {
        errno = EINVAL; return INVALID_HANDLE_VALUE;
    }
    if ((flags & O_ACCMODE) != O_WRONLY) access |= FILE_READ_DATA;
    if ((flags & O_ACCMODE) != O_RDONLY) access |= FILE_WRITE_DATA;
    if (flags & O_CREAT) {
        access |= FILE_WRITE_ATTRIBUTES | WRITE_DAC;
        disposition = flags & O_EXCL ? FX_NT_CREATE :
                      flags & O_TRUNC ? FX_NT_OVERWRITE_IF : FX_NT_OPEN_IF;
    } else if (flags & O_TRUNC) { errno = EINVAL; return INVALID_HANDLE_VALUE; }
    text.Buffer = (wchar_t *)name;
    text.Length = (USHORT)(length * sizeof(wchar_t));
    text.MaximumLength = text.Length;
    memset(&object, 0, sizeof(object));
    object.Length = sizeof(object);
    object.RootDirectory = parent;
    object.ObjectName = &text;
    object.Attributes = 0x40; /* OBJ_CASE_INSENSITIVE, subject to volume policy. */
    if ((flags & O_CREAT) && mode >= 0 && !(mode & 0077)) {
        security = private_descriptor();
        if (!security) { fx_win32_errno(GetLastError()); return INVALID_HANDLE_VALUE; }
        object.SecurityDescriptor = security;
    }
    status = create(&handle, access, &object, &io, NULL, FILE_ATTRIBUTE_NORMAL,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, disposition,
        FX_NT_SYNCHRONOUS | FX_NT_REPARSE | FX_NT_WRITE_THROUGH |
        (flags & O_DIRECTORY ? FX_NT_DIRECTORY : 0), NULL, 0);
    if (security) LocalFree(security);
    if (status < 0) { nt_error(status); return INVALID_HANDLE_VALUE; }
    BY_HANDLE_FILE_INFORMATION info;
    if (!GetFileInformationByHandle(handle, &info)) {
        DWORD error = GetLastError(); CloseHandle(handle); fx_win32_errno(error);
        return INVALID_HANDLE_VALUE;
    }
    attributes = info.dwFileAttributes;
    if ((!allow_reparse && (attributes & FILE_ATTRIBUTE_REPARSE_POINT)) ||
        ((flags & O_DIRECTORY) && !(attributes & FILE_ATTRIBUTE_DIRECTORY))) {
        CloseHandle(handle); errno = ELOOP; return INVALID_HANDLE_VALUE;
    }
    return handle;
}

static int absolute_wide(const char *path, wchar_t **out)
{
    wchar_t *wide = fx_win32_wide(path), *full;
    DWORD count;
    if (!wide) return fx_win32_errno(GetLastError());
    count = GetFullPathNameW(wide, 0, NULL, NULL);
    if (!count) { DWORD error = GetLastError(); free(wide); return fx_win32_errno(error); }
    full = malloc((size_t)count * sizeof(*full));
    if (!full) { free(wide); errno = ENOMEM; return -1; }
    if (!GetFullPathNameW(wide, count, full, NULL)) {
        DWORD error = GetLastError(); free(full); free(wide); return fx_win32_errno(error);
    }
    free(wide);
    for (wchar_t *p = full; *p; ++p) if (*p == L'/') *p = L'\\';
    *out = full;
    return 0;
}

/* Resolve components through held handles, never through a reconstructed path. */
static HANDLE walk_absolute(const char *path, int flags, int create_dirs, int mode)
{
    wchar_t *full = NULL, *cursor, *end;
    HANDLE current, next;
    size_t root_length;
    if (absolute_wide(path, &full) != 0) return INVALID_HANDLE_VALUE;
    if (full[0] && full[1] == L':' && full[2] == L'\\') root_length = 3;
    else if (full[0] == L'\\' && full[1] == L'\\') {
        cursor = wcschr(full + 2, L'\\');
        if (!cursor || !(end = wcschr(cursor + 1, L'\\'))) {
            root_length = wcslen(full);
        } else root_length = (size_t)(end - full);
    } else { free(full); errno = EINVAL; return INVALID_HANDLE_VALUE; }
    wchar_t saved = full[root_length];
    full[root_length] = 0;
    current = CreateFileW(full, FILE_LIST_DIRECTORY | FILE_READ_ATTRIBUTES |
        SYNCHRONIZE, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS |
        FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_WRITE_THROUGH, NULL);
    full[root_length] = saved;
    if (current == INVALID_HANDLE_VALUE) {
        DWORD error = GetLastError(); free(full); fx_win32_errno(error);
        return INVALID_HANDLE_VALUE;
    }
    cursor = full + root_length;
    while (*cursor == L'\\') ++cursor;
    if (!*cursor && (flags & O_CREAT) && (flags & O_EXCL)) {
        CloseHandle(current); free(full); errno = EEXIST; return INVALID_HANDLE_VALUE;
    }
    while (*cursor) {
        end = wcschr(cursor, L'\\');
        wchar_t delimiter = end ? *end : 0;
        if (end) *end = 0;
        wchar_t *after = end ? end + 1 : cursor + wcslen(cursor);
        while (*after == L'\\') ++after;
        int last = !*after;
        int child_flags = last ? flags : O_RDONLY | O_DIRECTORY | O_NOFOLLOW;
        if (create_dirs) child_flags |= O_DIRECTORY | O_CREAT;
        next = child_handle(current, cursor, child_flags, 0, 0, create_dirs ? mode : last ? mode : 0777);
        if (end) *end = delimiter;
        CloseHandle(current);
        if (next == INVALID_HANDLE_VALUE) { free(full); return next; }
        current = next;
        cursor = after;
    }
    free(full);
    return current;
}

int fx_win_open(const char *path, int flags, ...)
{
    int mode = -1;
    if (flags & O_CREAT) { va_list args; va_start(args, flags); mode = va_arg(args, int); va_end(args); }
    HANDLE handle = walk_absolute(path, flags, 0, mode);
    return handle == INVALID_HANDLE_VALUE ? -1 : descriptor(handle, flags);
}
int fx_win_open_directory(const char *path)
{
    return fx_win_open(path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW);
}
int fx_win_openat(int parent, const char *name, int flags, ...)
{
    wchar_t *wide;
    HANDLE handle;
    int mode = -1;
    if (flags & O_CREAT) { va_list args; va_start(args, flags); mode = va_arg(args, int); va_end(args); }
    if (parent == AT_FDCWD) return fx_win_open(name, flags, mode);
    wide = fx_win32_wide(name);
    if (!wide) return fx_win32_errno(GetLastError());
    handle = child_handle(handle_of(parent), wide, flags, 0, 0, mode);
    free(wide);
    return handle == INVALID_HANDLE_VALUE ? -1 : descriptor(handle, flags);
}

static int stat_handle(HANDLE handle, struct fx_win_stat *st)
{
    BY_HANDLE_FILE_INFORMATION info;
    ULARGE_INTEGER time;
    if (!GetFileInformationByHandle(handle, &info)) return fx_win32_errno(GetLastError());
    memset(st, 0, sizeof(*st));
    st->st_dev = info.dwVolumeSerialNumber;
    st->st_ino = ((uint64_t)info.nFileIndexHigh << 32) | info.nFileIndexLow;
    st->st_nlink = info.nNumberOfLinks;
    FILE_STANDARD_INFO standard;
    if (!GetFileInformationByHandleEx(handle, FileStandardInfo, &standard,
        sizeof(standard))) return fx_win32_errno(GetLastError());
    st->st_blocks = (standard.AllocationSize.QuadPart + 511) / 512;
    st->st_size = ((int64_t)info.nFileSizeHigh << 32) | info.nFileSizeLow;
    st->st_mode = info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT ? S_IFLNK :
                  info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ? S_IFDIR : S_IFREG;
    st->st_mode |= info.dwFileAttributes & FILE_ATTRIBUTE_READONLY ? 0444 : 0666;
    if (S_ISDIR(st->st_mode)) st->st_mode |= 0111;
    time.LowPart = info.ftLastWriteTime.dwLowDateTime;
    time.HighPart = info.ftLastWriteTime.dwHighDateTime;
    uint64_t unix_ticks = time.QuadPart - 116444736000000000ULL;
    st->st_mtime = (time_t)(unix_ticks / 10000000);
    st->st_mtim.tv_sec = st->st_mtime;
    st->st_mtim.tv_nsec = (long)((unix_ticks % 10000000) * 100);
    FILE_BASIC_INFO basic;
    if (!GetFileInformationByHandleEx(handle, FileBasicInfo, &basic, sizeof(basic)))
        return fx_win32_errno(GetLastError());
    time.QuadPart = (uint64_t)basic.ChangeTime.QuadPart;
    unix_ticks = time.QuadPart - 116444736000000000ULL;
    st->st_ctime = (time_t)(unix_ticks / 10000000);
    st->st_ctim.tv_sec = st->st_ctime;
    st->st_ctim.tv_nsec = (long)((unix_ticks % 10000000) * 100);
    return 0;
}
int fx_win_fstat(int fd, struct fx_win_stat *st) { return stat_handle(handle_of(fd), st); }
int fx_win_fstatat(int parent, const char *name, struct fx_win_stat *st, int flags)
{
    wchar_t *wide = fx_win32_wide(name);
    HANDLE handle;
    int rc;
    (void)flags;
    if (!wide) return fx_win32_errno(GetLastError());
    handle = child_handle(handle_of(parent), wide, O_RDONLY, 0, 1, -1);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) return -1;
    rc = stat_handle(handle, st);
    CloseHandle(handle);
    return rc;
}

static HANDLE path_handle(const char *path, ACCESS_MASK access, int allow_reparse)
{
    wchar_t *full = NULL, *slash;
    char *parent;
    HANDLE directory, result;
    if (absolute_wide(path, &full) != 0) return INVALID_HANDLE_VALUE;
    slash = wcsrchr(full, L'\\');
    if (!slash || !slash[1]) { free(full); errno = EINVAL; return INVALID_HANDLE_VALUE; }
    wchar_t saved = *slash;
    if (slash == full + 2 && full[1] == L':') ++slash;
    else *slash = 0;
    parent = fx_win32_utf8(full);
    if (slash == full + 3 && full[1] == L':') --slash;
    *slash = saved;
    if (!parent) { free(full); fx_win32_errno(GetLastError()); return INVALID_HANDLE_VALUE; }
    directory = walk_absolute(parent, O_RDONLY | O_DIRECTORY, 0, -1);
    free(parent);
    if (directory == INVALID_HANDLE_VALUE) { free(full); return directory; }
    result = child_handle(directory, slash + 1, O_RDONLY, access, allow_reparse, -1);
    CloseHandle(directory);
    free(full);
    return result;
}
int fx_win_lstat(const char *path, struct fx_win_stat *st)
{
    HANDLE handle = path_handle(path, 0, 1);
    int rc;
    if (handle == INVALID_HANDLE_VALUE) return -1;
    rc = stat_handle(handle, st);
    CloseHandle(handle);
    return rc;
}
int fx_win_stat(const char *path, struct fx_win_stat *st)
{
    wchar_t *wide = fx_win32_path(path);
    HANDLE handle;
    int rc;
    if (!wide) return fx_win32_errno(GetLastError());
    handle = CreateFileW(wide, FILE_READ_ATTRIBUTES, FILE_SHARE_READ |
        FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS, NULL);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) return fx_win32_errno(GetLastError());
    rc = stat_handle(handle, st); CloseHandle(handle); return rc;
}
int fx_win_mkdirat(int parent, const char *name, int mode)
{
    int fd = fx_win_openat(parent, name,
        O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CREAT | O_EXCL, mode);
    if (fd < 0) return -1;
    return fx_win_close(fd);
}
int fx_win_mkdir(const char *path, int mode)
{
    int fd = fx_win_open(path, O_RDONLY | O_DIRECTORY | O_CREAT | O_EXCL, mode);
    if (fd < 0) return -1;
    return fx_win_close(fd);
}
int fx_win_mkdirs_mode(const char *path, int mode, int sync)
{
    HANDLE handle = walk_absolute(path, O_RDONLY | O_DIRECTORY | O_CREAT, 1, mode);
    int fd, rc;
    if (handle == INVALID_HANDLE_VALUE) return -1;
    fd = descriptor(handle, O_RDONLY);
    if (fd < 0) return -1;
    rc = sync ? fx_win_fsync(fd) : 0;
    fx_win_close(fd);
    return rc;
}

int fx_win_mkdirs(const char *path, int sync)
{ return fx_win_mkdirs_mode(path, 0777, sync); }

static int set_information(HANDLE handle, void *bytes, ULONG count, int kind)
{
    IO_STATUS_BLOCK io;
    set_fn set = (set_fn)(void *)GetProcAddress(
        GetModuleHandleW(L"ntdll.dll"), "NtSetInformationFile");
    if (!set) { errno = ENOTSUP; return -1; }
    NTSTATUS status = set(handle, &io, bytes, count, (FILE_INFORMATION_CLASS)kind);
    return status < 0 ? nt_error(status) : 0;
}
struct name_information {
    BOOLEAN replace;
    HANDLE root;
    ULONG length;
    WCHAR name[1];
};
static int name_change(HANDLE source, HANDLE parent, const char *name,
                       int replace, int kind)
{
    wchar_t *wide = fx_win32_wide(name);
    struct name_information *info;
    size_t count;
    int rc;
    if (!wide) return fx_win32_errno(GetLastError());
    if (wcschr(wide, L'\\') || wcschr(wide, L'/') || wcschr(wide, L':') ||
        !wcscmp(wide, L".") || !wcscmp(wide, L"..")) {
        free(wide); errno = EINVAL; return -1;
    }
    count = offsetof(struct name_information, name) + wcslen(wide) * sizeof(*wide);
    info = calloc(1, count + sizeof(*wide));
    if (!info) { free(wide); errno = ENOMEM; return -1; }
    info->replace = (BOOLEAN)replace;
    if (kind == 65) *(ULONG *)(void *)info = (replace ? 1UL : 0UL) | 2UL;
    info->root = parent;
    info->length = (ULONG)(wcslen(wide) * sizeof(*wide));
    memcpy(info->name, wide, info->length);
    rc = set_information(source, info, (ULONG)count, kind);
    free(info); free(wide);
    return rc;
}
static int rename_relative(int olddir, const char *oldname, int newdir,
                           const char *newname, int replace)
{
    wchar_t *wide = fx_win32_wide(oldname);
    HANDLE source;
    int rc;
    if (!wide) return fx_win32_errno(GetLastError());
    source = child_handle(handle_of(olddir), wide, O_RDONLY, DELETE, 0, -1);
    free(wide);
    if (source == INVALID_HANDLE_VALUE) return -1;
    rc = name_change(source, handle_of(newdir), newname, replace, 65);
    CloseHandle(source);
    return rc;
}
int fx_win_renameat(int olddir, const char *oldname, int newdir, const char *newname)
{ return rename_relative(olddir, oldname, newdir, newname, 1); }
int fx_win_rename_exclusive(int olddir, const char *oldname, int newdir, const char *newname)
{ return rename_relative(olddir, oldname, newdir, newname, 0); }
int fx_win_link_fd(int source, int parent, const char *name)
{ return name_change(handle_of(source), handle_of(parent), name, 0, 11); }

static int remove_handle(HANDLE handle, int directory)
{
    BY_HANDLE_FILE_INFORMATION info;
    ULONG disposition = 1UL | 2UL | 0x10UL;
    if (!GetFileInformationByHandle(handle, &info)) return fx_win32_errno(GetLastError());
    if (!(info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) &&
        !!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != !!directory) {
        errno = directory ? ENOTDIR : EISDIR; return -1;
    }
    /* Delete this name without changing the shared readonly attribute of
       other hardlinks or denying existing readers their held object. */
    return set_information(handle, &disposition, sizeof(disposition), 64);
}
int fx_win_unlinkat(int parent, const char *name, int flags)
{
    wchar_t *wide = fx_win32_wide(name);
    HANDLE handle;
    int rc;
    if (!wide) return fx_win32_errno(GetLastError());
    handle = child_handle(handle_of(parent), wide, O_RDONLY,
        DELETE | FILE_WRITE_ATTRIBUTES, 1, -1);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) return -1;
    rc = remove_handle(handle, flags & AT_REMOVEDIR);
    CloseHandle(handle);
    return rc;
}
static int remove_path(const char *path, int directory)
{
    HANDLE handle = path_handle(path, DELETE | FILE_WRITE_ATTRIBUTES, 1);
    int rc;
    if (handle == INVALID_HANDLE_VALUE) return -1;
    rc = remove_handle(handle, directory);
    CloseHandle(handle);
    return rc;
}
int fx_win_unlink(const char *path) { return remove_path(path, 0); }
int fx_win_rmdir(const char *path) { return remove_path(path, 1); }

int fx_win_fsync(int fd)
{
    HANDLE handle = handle_of(fd);
    BY_HANDLE_FILE_INFORMATION info;
    if (!GetFileInformationByHandle(handle, &info)) return fx_win32_errno(GetLastError());
    if (FlushFileBuffers(handle)) return 0;
    DWORD error = GetLastError();
    if (!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && error == ERROR_ACCESS_DENIED) {
        HANDLE writable = ReOpenFile(handle, FILE_WRITE_DATA | SYNCHRONIZE,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, FILE_FLAG_WRITE_THROUGH);
        if (writable != INVALID_HANDLE_VALUE) {
            int rc = FlushFileBuffers(writable) ? 0 : fx_win32_errno(GetLastError());
            CloseHandle(writable); return rc;
        }
        return fx_win32_errno(GetLastError());
    }
    /* All directory mutations above are native write-through requests. NTFS
       commits their metadata with the request; it has no separate user-mode
       directory flush. Never suppress a regular-file flush failure. */
    if ((info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) &&
        (error == ERROR_INVALID_HANDLE || error == ERROR_ACCESS_DENIED)) return 0;
    return fx_win32_errno(error);
}
static HANDLE metadata_handle(int fd, ACCESS_MASK access)
{
    HANDLE handle = ReOpenFile(handle_of(fd), access | READ_CONTROL | SYNCHRONIZE,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_WRITE_THROUGH);
    if (handle == INVALID_HANDLE_VALUE) fx_win32_errno(GetLastError());
    return handle;
}
int fx_win_fchmod(int fd, mode_t mode)
{
    FILE_BASIC_INFO basic;
    HANDLE writable = metadata_handle(fd, FILE_WRITE_ATTRIBUTES |
        (!(mode & 0077) ? WRITE_DAC : 0));
    if (writable == INVALID_HANDLE_VALUE) return -1;
    if (!(mode & 0077)) {
        PSECURITY_DESCRIPTOR security = private_descriptor();
        BOOL present, defaulted;
        PACL dacl;
        if (!security) { CloseHandle(writable); return fx_win32_errno(GetLastError()); }
        if (!GetSecurityDescriptorDacl(security, &present, &dacl, &defaulted)) {
            DWORD error = GetLastError(); LocalFree(security); CloseHandle(writable);
            return fx_win32_errno(error);
        }
        DWORD error = SetSecurityInfo(writable, SE_FILE_OBJECT,
            DACL_SECURITY_INFORMATION | PROTECTED_DACL_SECURITY_INFORMATION,
            NULL, NULL, dacl, NULL);
        LocalFree(security);
        if (error != ERROR_SUCCESS) { CloseHandle(writable); return fx_win32_errno(error); }
    }
    if (!GetFileInformationByHandleEx(writable, FileBasicInfo, &basic, sizeof(basic))) {
        DWORD error = GetLastError(); CloseHandle(writable); return fx_win32_errno(error);
    }
    if (mode & 0222) basic.FileAttributes &= ~FILE_ATTRIBUTE_READONLY;
    else basic.FileAttributes |= FILE_ATTRIBUTE_READONLY;
    if (!basic.FileAttributes) basic.FileAttributes = FILE_ATTRIBUTE_NORMAL;
    int rc = SetFileInformationByHandle(writable, FileBasicInfo, &basic, sizeof(basic)) ?
        0 : fx_win32_errno(GetLastError());
    CloseHandle(writable); return rc;
}
int fx_win_chmod(const char *path, mode_t mode)
{
    HANDLE handle = path_handle(path, FILE_WRITE_ATTRIBUTES, 0);
    int fd, rc;
    if (handle == INVALID_HANDLE_VALUE) return -1;
    fd = descriptor(handle, O_RDONLY);
    if (fd < 0) return -1;
    rc = fx_win_fchmod(fd, mode);
    fx_win_close(fd);
    return rc;
}
static ssize_t positional_io(int fd, void *bytes, size_t count, int64_t offset, int writing)
{
    OVERLAPPED at;
    DWORD done = 0;
    if (offset < 0 || count > UINT32_MAX) { errno = EINVAL; return -1; }
    HANDLE file = ReOpenFile(handle_of(fd), SYNCHRONIZE |
        (writing ? FILE_WRITE_DATA : FILE_READ_DATA),
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        FILE_FLAG_OVERLAPPED | (writing ? FILE_FLAG_WRITE_THROUGH : 0));
    if (file == INVALID_HANDLE_VALUE) return fx_win32_errno(GetLastError());
    memset(&at, 0, sizeof(at));
    at.Offset = (DWORD)(uint64_t)offset;
    at.OffsetHigh = (DWORD)((uint64_t)offset >> 32);
    BOOL ok = writing ? WriteFile(file, bytes, (DWORD)count, &done, &at) :
                       ReadFile(file, bytes, (DWORD)count, &done, &at);
    if (!ok && GetLastError() == ERROR_IO_PENDING) ok = GetOverlappedResult(file, &at, &done, TRUE);
    DWORD error = ok ? ERROR_SUCCESS : GetLastError();
    CloseHandle(file);
    if (error == ERROR_HANDLE_EOF) return 0;
    return error == ERROR_SUCCESS ? (ssize_t)done : fx_win32_errno(error);
}
ssize_t fx_win_pread(int fd, void *bytes, size_t count, int64_t offset)
{ return positional_io(fd, bytes, count, offset, 0); }

int fx_win_ftruncate(int fd, int64_t size)
{
    FILE_END_OF_FILE_INFO end;
    end.EndOfFile.QuadPart = size;
    if (size < 0) { errno = EINVAL; return -1; }
    return SetFileInformationByHandle(handle_of(fd), FileEndOfFileInfo, &end,
        sizeof(end)) ? 0 : fx_win32_errno(GetLastError());
}

struct directory_lock { int fd; HANDLE mutex; struct directory_lock *next; };
static SRWLOCK lock_list_guard = SRWLOCK_INIT;
static struct directory_lock *directory_locks;
int fx_win_flock(int fd, int operation)
{
    HANDLE file = handle_of(fd);
    BY_HANDLE_FILE_INFORMATION info;
    OVERLAPPED range;
    if (!GetFileInformationByHandle(file, &info)) return fx_win32_errno(GetLastError());
    if (!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
        memset(&range, 0, sizeof(range));
        /* Unix advisory locks must not deny reads of the owner record. */
        range.Offset = 0xfffffffeUL;
        range.OffsetHigh = 0xffffffffUL;
        AcquireSRWLockExclusive(&lock_list_guard);
        struct directory_lock **link = &directory_locks, *held;
        while (*link && (*link)->fd != fd) link = &(*link)->next;
        held = *link;
        if (operation & LOCK_UN) {
            if (!held) { ReleaseSRWLockExclusive(&lock_list_guard); return 0; }
            if (!UnlockFileEx(file, 0, 1, 0, &range)) {
                DWORD error = GetLastError(); ReleaseSRWLockExclusive(&lock_list_guard);
                return fx_win32_errno(error);
            }
            *link = held->next;
            ReleaseSRWLockExclusive(&lock_list_guard); free(held); return 0;
        }
        if (held) { ReleaseSRWLockExclusive(&lock_list_guard); return 0; }
        ReleaseSRWLockExclusive(&lock_list_guard);
        DWORD flags = operation & LOCK_EX ? LOCKFILE_EXCLUSIVE_LOCK : 0;
        if (operation & LOCK_NB) flags |= LOCKFILE_FAIL_IMMEDIATELY;
        if (LockFileEx(file, flags, 0, 1, 0, &range)) {
            held = malloc(sizeof(*held));
            if (!held) { UnlockFileEx(file, 0, 1, 0, &range); errno = ENOMEM; return -1; }
            held->fd = fd; held->mutex = NULL;
            AcquireSRWLockExclusive(&lock_list_guard);
            held->next = directory_locks; directory_locks = held;
            ReleaseSRWLockExclusive(&lock_list_guard); return 0;
        }
        DWORD error = GetLastError();
        if (error == ERROR_LOCK_VIOLATION) { errno = EWOULDBLOCK; return -1; }
        return fx_win32_errno(error);
    }
    if (operation & LOCK_UN) {
        AcquireSRWLockExclusive(&lock_list_guard);
        struct directory_lock **link = &directory_locks, *held;
        while (*link && (*link)->fd != fd) link = &(*link)->next;
        held = *link;
        if (!held) { ReleaseSRWLockExclusive(&lock_list_guard); errno = EINVAL; return -1; }
        if (!ReleaseMutex(held->mutex)) {
            DWORD error = GetLastError(); ReleaseSRWLockExclusive(&lock_list_guard);
            return fx_win32_errno(error);
        }
        *link = held->next;
        ReleaseSRWLockExclusive(&lock_list_guard);
        CloseHandle(held->mutex); free(held);
        return 0;
    }
    if (!(operation & LOCK_EX)) { errno = ENOTSUP; return -1; }
    wchar_t name[96];
    swprintf(name, 96, L"Global\\FxStoreDir-%08lx-%08lx%08lx",
        (unsigned long)info.dwVolumeSerialNumber,
        (unsigned long)info.nFileIndexHigh, (unsigned long)info.nFileIndexLow);
    HANDLE mutex = CreateMutexW(NULL, FALSE, name);
    if (!mutex) return fx_win32_errno(GetLastError());
    DWORD waited = WaitForSingleObject(mutex, operation & LOCK_NB ? 0 : INFINITE);
    if (waited != WAIT_OBJECT_0 && waited != WAIT_ABANDONED) {
        DWORD error = GetLastError(); CloseHandle(mutex);
        if (waited == WAIT_TIMEOUT) { errno = EWOULDBLOCK; return -1; }
        return fx_win32_errno(error);
    }
    struct directory_lock *held = malloc(sizeof(*held));
    if (!held) { ReleaseMutex(mutex); CloseHandle(mutex); errno = ENOMEM; return -1; }
    held->fd = fd; held->mutex = mutex;
    AcquireSRWLockExclusive(&lock_list_guard);
    held->next = directory_locks; directory_locks = held;
    ReleaseSRWLockExclusive(&lock_list_guard);
    return 0;
}
int fx_win_close(int fd)
{
    int locked = 0;
    AcquireSRWLockShared(&lock_list_guard);
    for (struct directory_lock *p = directory_locks; p; p = p->next)
        if (p->fd == fd) { locked = 1; break; }
    ReleaseSRWLockShared(&lock_list_guard);
    if (locked && fx_win_flock(fd, LOCK_UN) != 0) return -1;
    return _close(fd);
}

struct fx_win_directory {
    int fd, first, refill;
    size_t offset;
    long position;
    _Alignas(8) unsigned char buffer[65536];
    struct fx_win_dirent entry;
};
fx_win_directory *fx_win_fdopendir(int fd)
{
    struct fx_win_stat st;
    if (fx_win_fstat(fd, &st) != 0 || !S_ISDIR(st.st_mode)) { errno = ENOTDIR; return NULL; }
    fx_win_directory *directory = calloc(1, sizeof(*directory));
    if (!directory) { errno = ENOMEM; return NULL; }
    directory->fd = fd; directory->first = 1; directory->refill = 1;
    return directory;
}
fx_win_directory *fx_win_opendir(const char *path)
{
    int fd = fx_win_open_directory(path);
    fx_win_directory *directory;
    if (fd < 0) return NULL;
    directory = fx_win_fdopendir(fd);
    if (!directory) fx_win_close(fd);
    return directory;
}
struct fx_win_dirent *fx_win_readdir(fx_win_directory *directory)
{
    FILE_ID_BOTH_DIR_INFO *info;
    wchar_t *name;
    char *utf8;
    if (directory->refill) {
        FILE_INFO_BY_HANDLE_CLASS kind = directory->first ?
            FileIdBothDirectoryRestartInfo : FileIdBothDirectoryInfo;
        if (!GetFileInformationByHandleEx(handle_of(directory->fd), kind,
            directory->buffer, sizeof(directory->buffer))) {
            DWORD error = GetLastError();
            if (error == ERROR_NO_MORE_FILES) errno = 0;
            else fx_win32_errno(error);
            return NULL;
        }
        directory->first = 0; directory->offset = 0; directory->refill = 0;
    }
    info = (FILE_ID_BOTH_DIR_INFO *)(directory->buffer + directory->offset);
    if (info->FileNameLength > sizeof(directory->buffer) - directory->offset -
        offsetof(FILE_ID_BOTH_DIR_INFO, FileName)) { errno = EIO; return NULL; }
    size_t chars = info->FileNameLength / sizeof(wchar_t);
    name = calloc(chars + 1, sizeof(*name));
    if (!name) { errno = ENOMEM; return NULL; }
    memcpy(name, info->FileName, info->FileNameLength);
    utf8 = fx_win32_utf8(name); free(name);
    if (!utf8) { fx_win32_errno(GetLastError()); return NULL; }
    if (strlen(utf8) >= sizeof(directory->entry.d_name)) {
        free(utf8); errno = ENAMETOOLONG; return NULL;
    }
    strcpy(directory->entry.d_name, utf8); free(utf8);
    if (info->NextEntryOffset) directory->offset += info->NextEntryOffset;
    else directory->refill = 1;
    ++directory->position;
    return &directory->entry;
}
int fx_win_dirfd(fx_win_directory *directory) { return directory->fd; }
int fx_win_closedir(fx_win_directory *directory)
{ int rc = fx_win_close(directory->fd); free(directory); return rc; }

char *fx_win_getcwd(char *out, size_t capacity)
{
    DWORD count = GetCurrentDirectoryW(0, NULL);
    wchar_t *wide;
    char *utf8;
    if (!count) { fx_win32_errno(GetLastError()); return NULL; }
    wide = malloc((size_t)count * sizeof(*wide));
    if (!wide) { errno = ENOMEM; return NULL; }
    if (!GetCurrentDirectoryW(count, wide)) { free(wide); fx_win32_errno(GetLastError()); return NULL; }
    utf8 = fx_win32_utf8(wide); free(wide);
    if (!utf8) { fx_win32_errno(GetLastError()); return NULL; }
    if (strlen(utf8) >= capacity) { free(utf8); errno = ERANGE; return NULL; }
    for (char *p = utf8; *p; ++p) if (*p == '\\') *p = '/';
    strcpy(out, utf8); free(utf8); return out;
}
int fx_win_resolve_root(const char *path, char *out, size_t capacity)
{
    wchar_t *wide = NULL;
    char *utf8;
    if (absolute_wide(path, &wide) != 0) return -1;
    utf8 = fx_win32_utf8(wide); free(wide);
    if (!utf8) return fx_win32_errno(GetLastError());
    if (strlen(utf8) >= capacity) { free(utf8); errno = ENAMETOOLONG; return -1; }
    for (char *p = utf8; *p; ++p) if (*p == '\\') *p = '/';
    strcpy(out, utf8); free(utf8); return 0;
}
char *fx_win_realpath(const char *path, char *out)
{
    wchar_t *wide = fx_win32_path(path), *final;
    HANDLE handle;
    char *utf8, *result;
    DWORD count;
    if (!wide) { fx_win32_errno(GetLastError()); return NULL; }
    handle = CreateFileW(wide, FILE_READ_ATTRIBUTES, FILE_SHARE_READ |
        FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS, NULL);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) { fx_win32_errno(GetLastError()); return NULL; }
    count = GetFinalPathNameByHandleW(handle, NULL, 0, FILE_NAME_NORMALIZED);
    final = malloc(((size_t)count + 1) * sizeof(*final));
    if (!count || !final) {
        DWORD error = GetLastError(); CloseHandle(handle); free(final);
        fx_win32_errno(count ? ERROR_NOT_ENOUGH_MEMORY : error); return NULL;
    }
    if (!GetFinalPathNameByHandleW(handle, final, count + 1, FILE_NAME_NORMALIZED)) {
        DWORD error = GetLastError(); CloseHandle(handle); free(final);
        fx_win32_errno(error); return NULL;
    }
    CloseHandle(handle);
    if (!wcsncmp(final, L"\\\\?\\UNC\\", 8)) {
        final[6] = L'\\'; utf8 = fx_win32_utf8(final + 6);
    } else utf8 = fx_win32_utf8(!wcsncmp(final, L"\\\\?\\", 4) ? final + 4 : final);
    free(final);
    if (!utf8) { fx_win32_errno(GetLastError()); return NULL; }
    for (char *p = utf8; *p; ++p) if (*p == '\\') *p = '/';
    if (!out) return utf8;
    if (strlen(utf8) >= 4096) { free(utf8); errno = ENAMETOOLONG; return NULL; }
    result = strcpy(out, utf8); free(utf8); return result;
}
static LONG temporary_serial;
static int create_temporary(char *path, int directory)
{
    size_t length = strlen(path);
    if (length < 6 || strcmp(path + length - 6, "XXXXXX")) { errno = EINVAL; return -1; }
    for (int attempt = 0; attempt < 128; ++attempt) {
        unsigned long serial = (unsigned long)InterlockedIncrement(&temporary_serial);
        snprintf(path + length - 6, 7, "%06lx", (serial ^ GetCurrentProcessId()) & 0xffffff);
        int fd = fx_win_open(path, O_RDWR | O_CREAT | O_EXCL |
            (directory ? O_DIRECTORY : 0), 0600);
        if (fd >= 0) return fd;
        if (errno != EEXIST) return -1;
    }
    errno = EEXIST; return -1;
}
char *fx_win_mkdtemp(char *path)
{
    int fd = create_temporary(path, 1);
    if (fd < 0) return NULL;
    fx_win_close(fd); return path;
}
int fx_win_mkstemp(char *path) { return create_temporary(path, 0); }
int fx_win_rename(const char *source, const char *destination)
{
    HANDLE held = path_handle(source, DELETE, 0);
    wchar_t *full = NULL, *slash;
    char *parent = NULL, *name = NULL;
    int directory = -1, rc = -1;
    if (held == INVALID_HANDLE_VALUE) return -1;
    if (absolute_wide(destination, &full) != 0) goto done;
    slash = wcsrchr(full, L'\\');
    if (!slash || !slash[1]) { errno = EINVAL; goto done; }
    name = fx_win32_utf8(slash + 1);
    if (slash == full + 2 && full[1] == L':') slash[1] = 0;
    else *slash = 0;
    parent = fx_win32_utf8(full);
    if (!name || !parent) { fx_win32_errno(GetLastError()); goto done; }
    directory = fx_win_open_directory(parent);
    if (directory < 0) goto done;
    /* POSIX replacement keeps pre-existing readers bound to the old file. */
    rc = name_change(held, handle_of(directory), name, 1, 65);
done:
    if (directory >= 0) fx_win_close(directory);
    CloseHandle(held); free(full); free(parent); free(name); return rc;
}
void fx_win_rewinddir(fx_win_directory *directory)
{ directory->first = 1; directory->refill = 1; directory->position = 0; }
void fx_win_seekdir(fx_win_directory *directory, long position)
{
    fx_win_rewinddir(directory);
    while (directory->position < position && fx_win_readdir(directory)) {}
}
long fx_win_telldir(fx_win_directory *directory) { return directory->position; }
int fx_win_futimens(int fd, const struct timespec times[2])
{
    FILETIME converted[2], *selected[2] = {NULL, NULL};
    for (int i = 0; i < 2; ++i) {
        if (times[i].tv_nsec == UTIME_OMIT) continue;
        if (times[i].tv_nsec == UTIME_NOW) GetSystemTimeAsFileTime(&converted[i]);
        else {
            if (times[i].tv_nsec < 0 || times[i].tv_nsec >= 1000000000L) {
                errno = EINVAL; return -1;
            }
            uint64_t ticks = (uint64_t)times[i].tv_sec * 10000000ULL +
                (uint64_t)times[i].tv_nsec / 100 + 116444736000000000ULL;
            converted[i].dwLowDateTime = (DWORD)ticks;
            converted[i].dwHighDateTime = (DWORD)(ticks >> 32);
        }
        selected[i] = &converted[i];
    }
    HANDLE writable = metadata_handle(fd, FILE_WRITE_ATTRIBUTES);
    if (writable == INVALID_HANDLE_VALUE) return -1;
    int rc = SetFileTime(writable, NULL, selected[0], selected[1]) ? 0 :
        fx_win32_errno(GetLastError());
    CloseHandle(writable); return rc;
}
ssize_t fx_win_getline(char **line, size_t *capacity, FILE *file)
{
    size_t used = 0;
    int byte;
    while ((byte = fgetc(file)) != EOF) {
        if (used + 2 > *capacity) {
            size_t next = *capacity ? *capacity * 2 : 256;
            if (next < *capacity) { errno = ENOMEM; return -1; }
            char *grown = realloc(*line, next);
            if (!grown) { errno = ENOMEM; return -1; }
            *line = grown; *capacity = next;
        }
        (*line)[used++] = (char)byte;
        if (byte == '\n') break;
    }
    if (!used) return -1;
    (*line)[used] = 0;
    return (ssize_t)used;
}
int fx_win_current_owned(int fd)
{
    HANDLE handle = handle_of(fd);
    BY_HANDLE_FILE_INFORMATION info;
    PSECURITY_DESCRIPTOR security = NULL;
    PSID owner = NULL;
    if (!GetFileInformationByHandle(handle, &info)) return fx_win32_errno(GetLastError());
    if (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) { errno = ELOOP; return 0; }
    DWORD error = GetSecurityInfo(handle, SE_FILE_OBJECT, OWNER_SECURITY_INFORMATION,
        &owner, NULL, NULL, NULL, &security);
    if (error != ERROR_SUCCESS) return fx_win32_errno(error);
    TOKEN_USER *user = current_user();
    if (!user) { LocalFree(security); return fx_win32_errno(GetLastError()); }
    int owned = owner && EqualSid(owner, user->User.Sid);
    free(user); LocalFree(security);
    if (!owned) errno = EACCES;
    return owned;
}


int fx_win_private_owned(int fd)
{
    HANDLE handle = handle_of(fd);
    BY_HANDLE_FILE_INFORMATION info;
    PSECURITY_DESCRIPTOR security = NULL;
    PSID owner = NULL;
    PACL dacl = NULL;
    TOKEN_USER *user;
    int private = 0;
    if (!GetFileInformationByHandle(handle, &info)) return fx_win32_errno(GetLastError());
    if (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) { errno = ELOOP; return 0; }
    DWORD error = GetSecurityInfo(handle, SE_FILE_OBJECT,
        OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
        &owner, NULL, &dacl, NULL, &security);
    if (error != ERROR_SUCCESS) return fx_win32_errno(error);
    user = current_user();
    if (!user) { LocalFree(security); return fx_win32_errno(GetLastError()); }
    if (!owner || !EqualSid(owner, user->User.Sid) || !dacl) goto done;
    for (DWORD i = 0; i < dacl->AceCount; ++i) {
        ACE_HEADER *header;
        if (!GetAce(dacl, i, (void **)&header)) goto done;
        if (header->AceType == ACCESS_DENIED_ACE_TYPE) continue;
        if (header->AceType != ACCESS_ALLOWED_ACE_TYPE) goto done;
        ACCESS_ALLOWED_ACE *ace = (ACCESS_ALLOWED_ACE *)header;
        PSID sid = (PSID)&ace->SidStart;
        if (!EqualSid(sid, user->User.Sid) &&
            !IsWellKnownSid(sid, WinLocalSystemSid) &&
            !IsWellKnownSid(sid, WinBuiltinAdministratorsSid)) goto done;
    }
    private = 1;
done:
    free(user); LocalFree(security);
    if (!private) errno = EACCES;
    return private;
}
ssize_t fx_win_pwrite(int fd, const void *bytes, size_t count, int64_t offset)
{ return positional_io(fd, (void *)bytes, count, offset, 1); }

ssize_t fx_win_readlinkat(int parent, const char *name, char *out, size_t capacity)
{
    wchar_t *wide = fx_win32_wide(name);
    HANDLE handle;
    unsigned char data[16384];
    DWORD count;
    int result = -1;
    if (!wide) return fx_win32_errno(GetLastError());
    handle = child_handle(handle_of(parent), wide, O_RDONLY, 0, 1, -1);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) return -1;
    if (!DeviceIoControl(handle, FSCTL_GET_REPARSE_POINT, NULL, 0,
        data, sizeof(data), &count, NULL)) {
        DWORD error = GetLastError(); CloseHandle(handle); return fx_win32_errno(error);
    }
    CloseHandle(handle);
    if (count < 16) { errno = EIO; return -1; }
    ULONG tag; USHORT substitute_offset, substitute_size, print_offset, print_size;
    memcpy(&tag, data, 4);
    memcpy(&substitute_offset, data + 8, 2); memcpy(&substitute_size, data + 10, 2);
    memcpy(&print_offset, data + 12, 2); memcpy(&print_size, data + 14, 2);
    size_t base = tag == IO_REPARSE_TAG_SYMLINK ? 20 :
                  tag == IO_REPARSE_TAG_MOUNT_POINT ? 16 : 0;
    if (!base) { errno = ENOTSUP; return -1; }
    size_t offset = print_size ? print_offset : substitute_offset;
    size_t length = print_size ? print_size : substitute_size;
    if (base + offset + length > count || length % sizeof(wchar_t)) {
        errno = EIO; return -1;
    }
    wide = calloc(length / sizeof(wchar_t) + 1, sizeof(*wide));
    if (!wide) { errno = ENOMEM; return -1; }
    memcpy(wide, data + base + offset, length);
    wchar_t *text = !wcsncmp(wide, L"\\??\\", 4) ? wide + 4 : wide;
    char *utf8 = fx_win32_utf8(text); free(wide);
    if (!utf8) return fx_win32_errno(GetLastError());
    for (char *p = utf8; *p; ++p) if (*p == '\\') *p = '/';
    size_t bytes = strlen(utf8);
    if (bytes > capacity) bytes = capacity;
    memcpy(out, utf8, bytes); free(utf8); result = (int)bytes;
    return (ssize_t)result;
}
int fx_win_symlink(const char *target, const char *path)
{
    wchar_t *wide_target = fx_win32_wide(target), *wide_path = fx_win32_path(path);
    DWORD flags = SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;
    if (!wide_target || !wide_path) {
        free(wide_target); free(wide_path); return fx_win32_errno(GetLastError());
    }
    DWORD attributes = GetFileAttributesW(wide_target);
    if (attributes != INVALID_FILE_ATTRIBUTES && (attributes & FILE_ATTRIBUTE_DIRECTORY))
        flags |= SYMBOLIC_LINK_FLAG_DIRECTORY;
    int rc = CreateSymbolicLinkW(wide_path, wide_target, flags) ? 0 :
        fx_win32_errno(GetLastError());
    free(wide_target); free(wide_path); return rc;
}
int fx_win_faccessat(int parent, const char *name, int mode, int flags)
{
    wchar_t *wide = fx_win32_wide(name);
    HANDLE handle;
    (void)flags;
    if (!wide) return fx_win32_errno(GetLastError());
    handle = child_handle(handle_of(parent), wide, mode & 2 ? O_RDWR : O_RDONLY,
        mode & 1 ? FILE_EXECUTE : 0, 0, -1);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) return -1;
    CloseHandle(handle); return 0;
}
#endif
