/* Native Windows implementation of the Fx system boundary. */
#include "fx_win32.h"
#include "fx_job_win32.h"
#include <io.h>
#include <fcntl.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/stat.h>

#define strncasecmp _strnicmp

wchar_t *fx_win32_utf16(const char *text)
{
    wchar_t *wide = fx_win32_wide(text);
    if (!wide) fx_win32_errno(GetLastError());
    return wide;
}

static wchar_t *fx_win32_filename(const char *text)
{
    wchar_t *wide = fx_win32_path(text);
    if (!wide) fx_win32_errno(GetLastError());
    return wide;
}

/* Quote every argument using the Microsoft C runtime's argv rules. The
 * caller supplies the complete packed byte extent so malformed input cannot
 * read into another object. CreateProcessW's limit includes the final NUL. */
wchar_t *fx_win32_command_line(const char *packed, int length, int count)
{
    wchar_t *line, *argument;
    const char *cursor = packed, *end, *zero;
    size_t used = 0, i, slashes, n;
    int index;
    if (!packed || length <= 0 || count <= 0) { errno = EINVAL; return NULL; }
    end = packed + length;
    line = malloc(32767 * sizeof(*line));
    if (!line) { errno = ENOMEM; return NULL; }
    for (index = 0; index < count; ++index) {
        zero = memchr(cursor, 0, (size_t)(end - cursor));
        if (!zero) { errno = EINVAL; goto failed; }
        argument = fx_win32_utf16(cursor);
        if (!argument) goto failed;
        n = wcslen(argument);
        if (used + 3 >= 32767) { free(argument); errno = E2BIG; goto failed; }
        if (index) line[used++] = L' ';
        line[used++] = L'"';
        for (i = 0; i < n;) {
            slashes = 0;
            while (i < n && argument[i] == L'\\') { ++slashes; ++i; }
            if (i == n || argument[i] == L'"') slashes *= 2;
            if (used + slashes + 3 >= 32767) {
                free(argument); errno = E2BIG; goto failed;
            }
            while (slashes > 0) { line[used++] = L'\\'; --slashes; }
            if (i < n) {
                if (argument[i] == L'"') line[used++] = L'\\';
                line[used++] = argument[i++];
            }
        }
        line[used++] = L'"';
        free(argument);
        cursor = zero + 1;
    }
    if (cursor != end) { errno = EINVAL; goto failed; }
    line[used] = 0;
    return line;
failed:
    free(line);
    return NULL;
}

static int fx_win32_spawn(const char *packed, int count, HANDLE out, HANDLE err,
                          PROCESS_INFORMATION *process, HANDLE *job)
{
    STARTUPINFOEXW startup;
    SIZE_T extent = 0;
    HANDLE inherited[3], input = INVALID_HANDLE_VALUE;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    SECURITY_ATTRIBUTES security = {sizeof(security), NULL, TRUE};
    wchar_t *command;
    const char *cursor = packed;
    size_t bytes = 0, n;
    DWORD failure;
    int i, rc = -1;
    if (!packed || count <= 0 || count > 32767) { errno = EINVAL; return -1; }
    for (i = 0; i < count; ++i) {
        n = strlen(cursor) + 1;
        if (n > INT_MAX - bytes) { errno = E2BIG; return -1; }
        bytes += n; cursor += n;
    }
    command = fx_win32_command_line(packed, (int)bytes, count);
    if (!command) return -1;
    memset(&startup, 0, sizeof(startup));
    memset(process, 0, sizeof(*process));
    startup.StartupInfo.cb = sizeof(startup);
    *job = CreateJobObjectW(NULL, NULL);
    if (!*job) goto failed;
    memset(&limits, 0, sizeof(limits));
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(*job, JobObjectExtendedLimitInformation,
                                 &limits, sizeof(limits))) goto failed;
    if (!DuplicateHandle(GetCurrentProcess(), GetStdHandle(STD_INPUT_HANDLE),
                          GetCurrentProcess(), &input, 0, TRUE,
                          DUPLICATE_SAME_ACCESS))
        input = CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                            &security, OPEN_EXISTING, 0, NULL);
    if (input == INVALID_HANDLE_VALUE) goto failed;
    inherited[0] = input; inherited[1] = out; inherited[2] = err;
    InitializeProcThreadAttributeList(NULL, 2, 0, &extent);
    startup.lpAttributeList = malloc(extent);
    if (!startup.lpAttributeList) { SetLastError(ERROR_NOT_ENOUGH_MEMORY); goto failed; }
    if (!InitializeProcThreadAttributeList(startup.lpAttributeList, 2, 0, &extent))
        goto free_attributes;
    if (!UpdateProcThreadAttribute(startup.lpAttributeList, 0,
            PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited,
            (out == err ? 2 : 3) * sizeof(HANDLE), NULL, NULL))
        goto delete_attributes;
    if (!UpdateProcThreadAttribute(startup.lpAttributeList, 0,
            PROC_THREAD_ATTRIBUTE_JOB_LIST, job, sizeof(*job), NULL, NULL))
        goto delete_attributes;
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = input;
    startup.StartupInfo.hStdOutput = out;
    startup.StartupInfo.hStdError = err;
    if (CreateProcessW(NULL, command, NULL, NULL, TRUE,
            EXTENDED_STARTUPINFO_PRESENT, NULL, NULL, &startup.StartupInfo, process))
        rc = 0;
delete_attributes:
    failure = GetLastError();
    DeleteProcThreadAttributeList(startup.lpAttributeList);
    SetLastError(failure);
free_attributes:
    failure = GetLastError(); free(startup.lpAttributeList); SetLastError(failure);
failed:
    failure = GetLastError();
    if (input != INVALID_HANDLE_VALUE) CloseHandle(input);
    free(command);
    if (rc) {
        if (*job) CloseHandle(*job);
        *job = NULL;
        fx_win32_errno(failure);
        return failure == ERROR_FILE_NOT_FOUND || failure == ERROR_PATH_NOT_FOUND ? 127 : 126;
    }
    CloseHandle(process->hThread);
    process->hThread = NULL;
    return 0;
}

/* Both streams are drained even after a caller's buffer is full. */
int fx_c_exec(const char *packed, int count, char *out, int *out_len,
              char *err, int *err_len)
{
    SECURITY_ATTRIBUTES security = {sizeof(security), NULL, TRUE};
    PROCESS_INFORMATION process;
    HANDLE reads[2] = {NULL, NULL}, writes[2] = {NULL, NULL}, job = NULL;
    char *buffers[2] = {out, err}, discard[4096];
    int capacity[2] = {*out_len, *err_len}, used[2] = {0, 0}, done[2] = {0, 0};
    DWORD available, got, take, code = 0, waited;
    void *tracker = NULL;
    int i, progress, rc = -1, drained = 0, failure;
    *out_len = 0; *err_len = 0;
    if (capacity[0] < 0 || capacity[1] < 0) { errno = EINVAL; return -1; }
    for (i = 0; i < 2; ++i) {
        if (!CreatePipe(&reads[i], &writes[i], &security, 0) ||
            !SetHandleInformation(reads[i], HANDLE_FLAG_INHERIT, 0)) {
            fx_win32_errno(GetLastError()); goto cleanup;
        }
    }
    rc = fx_win32_spawn(packed, count, writes[0], writes[1], &process, &job);
    CloseHandle(writes[0]); writes[0] = NULL;
    CloseHandle(writes[1]); writes[1] = NULL;
    if (rc) goto cleanup;
    while (!done[0] || !done[1]) {
        failure = fx_win32_job_tree_poll(job, &tracker);
        if (failure) { errno = failure; rc = -1; break; }
        waited = WaitForSingleObject(process.hProcess, 0);
        if (waited == WAIT_FAILED) {
            fx_win32_errno(GetLastError()); rc = -1; break;
        }
        if (!drained && waited == WAIT_OBJECT_0) {
            if (!GetExitCodeProcess(process.hProcess, &code)) {
                fx_win32_errno(GetLastError()); rc = -1; break;
            }
            failure = fx_win32_job_tree_drain(job, &tracker, 5000);
            if (failure) { errno = failure; rc = -1; break; }
            drained = 1;
        }
        progress = 0;
        for (i = 0; i < 2; ++i) {
            if (done[i]) continue;
            if (!PeekNamedPipe(reads[i], NULL, 0, NULL, &available, NULL)) {
                if (GetLastError() != ERROR_BROKEN_PIPE) {
                    fx_win32_errno(GetLastError()); rc = -1;
                }
                done[i] = 1; continue;
            }
            if (!available) continue;
            take = (DWORD)(capacity[i] - used[i]);
            if (!take || take > sizeof(discard)) take = sizeof(discard);
            if (take > available) take = available;
            if (!ReadFile(reads[i], used[i] < capacity[i] ? buffers[i] + used[i] :
                    discard, take, &got, NULL)) {
                fx_win32_errno(GetLastError()); rc = -1; done[i] = 1; continue;
            }
            if (used[i] < capacity[i]) used[i] += (int)got;
            progress = 1;
        }
        if (rc) break;
        if (!progress) Sleep(1);
    }
    while (!rc && !drained) {
        failure = fx_win32_job_tree_poll(job, &tracker);
        if (failure) { errno = failure; rc = -1; break; }
        waited = WaitForSingleObject(process.hProcess, 1);
        if (waited == WAIT_FAILED) {
            fx_win32_errno(GetLastError()); rc = -1; break;
        }
        if (waited != WAIT_OBJECT_0) continue;
        if (!GetExitCodeProcess(process.hProcess, &code)) {
            fx_win32_errno(GetLastError()); rc = -1; break;
        }
        failure = fx_win32_job_tree_drain(job, &tracker, 5000);
        if (failure) { errno = failure; rc = -1; break; }
        drained = 1;
    }
    if (!rc) rc = (int)code;
    if (!drained) (void)fx_win32_job_tree_drain(job, &tracker, 5000);
    fx_win32_job_tree_free(&tracker);
    CloseHandle(process.hProcess);
    CloseHandle(job);
cleanup:
    for (i = 0; i < 2; ++i) {
        if (reads[i]) CloseHandle(reads[i]);
        if (writes[i]) CloseHandle(writes[i]);
    }
    *out_len = used[0]; *err_len = used[1];
    return rc;
}

int fx_c_exec_silent(const char *packed, int count)
{
    SECURITY_ATTRIBUTES security = {sizeof(security), NULL, TRUE};
    PROCESS_INFORMATION process;
    HANDLE job = NULL;
    HANDLE sink = CreateFileW(L"NUL", GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
                              &security, OPEN_EXISTING, 0, NULL);
    DWORD code, waited;
    void *tracker = NULL;
    int rc, failure;
    if (sink == INVALID_HANDLE_VALUE) return fx_win32_errno(GetLastError());
    rc = fx_win32_spawn(packed, count, sink, sink, &process, &job);
    CloseHandle(sink);
    if (rc) return rc;
    for (;;) {
        failure = fx_win32_job_tree_poll(job, &tracker);
        if (failure) { errno = failure; rc = -1; break; }
        waited = WaitForSingleObject(process.hProcess, 1);
        if (waited == WAIT_FAILED) {
            fx_win32_errno(GetLastError()); rc = -1; break;
        }
        if (waited != WAIT_OBJECT_0) continue;
        if (GetExitCodeProcess(process.hProcess, &code)) rc = (int)code;
        else { fx_win32_errno(GetLastError()); rc = -1; }
        break;
    }
    failure = fx_win32_job_tree_drain(job, &tracker, 5000);
    if (failure) { errno = failure; rc = -1; }
    fx_win32_job_tree_free(&tracker);
    CloseHandle(process.hProcess);
    CloseHandle(job);
    return rc;
}

static HANDLE fx_win32_file(const char *path, DWORD access, DWORD creation)
{
    wchar_t *wide = fx_win32_filename(path);
    HANDLE handle;
    DWORD failure;
    if (!wide) return INVALID_HANDLE_VALUE;
    handle = CreateFileW(wide, access, FILE_SHARE_READ | FILE_SHARE_WRITE |
                         FILE_SHARE_DELETE, NULL, creation,
                         FILE_FLAG_BACKUP_SEMANTICS, NULL);
    failure = GetLastError(); free(wide);
    if (handle == INVALID_HANDLE_VALUE) fx_win32_errno(failure);
    return handle;
}

int fx_c_file_read(const char *path, char *content, int *length)
{
    HANDLE file;
    DWORD got;
    int capacity = *length, total = 0;
    *length = 0;
    if (capacity < 0) { errno = EINVAL; return -1; }
    file = fx_win32_file(path, GENERIC_READ, OPEN_EXISTING);
    if (file == INVALID_HANDLE_VALUE) return -1;
    while (total < capacity) {
        if (!ReadFile(file, content + total, (DWORD)(capacity - total), &got, NULL)) {
            DWORD failure = GetLastError(); CloseHandle(file);
            return fx_win32_errno(failure);
        }
        if (!got) break;
        total += (int)got;
    }
    CloseHandle(file); *length = total;
    return 0;
}

int fx_c_file_write(const char *path, const char *content, int length)
{
    HANDLE file;
    DWORD written;
    int total = 0;
    if (length < 0) { errno = EINVAL; return -1; }
    file = fx_win32_file(path, GENERIC_WRITE, CREATE_ALWAYS);
    if (file == INVALID_HANDLE_VALUE) return -1;
    while (total < length) {
        if (!WriteFile(file, content + total, (DWORD)(length - total), &written, NULL) ||
            !written) {
            DWORD failure = GetLastError(); CloseHandle(file);
            return fx_win32_errno(failure ? failure : ERROR_WRITE_FAULT);
        }
        total += (int)written;
    }
    return CloseHandle(file) ? 0 : fx_win32_errno(GetLastError());
}

int fx_c_pid(void) { return (int)GetCurrentProcessId(); }

int fx_c_kill(int pid, int signal_number)
{
    HANDLE process;
    DWORD access = signal_number ? PROCESS_TERMINATE : PROCESS_QUERY_LIMITED_INFORMATION;
    DWORD code;
    int rc;
    if (pid <= 0 || (signal_number != 0 && signal_number != 9 && signal_number != 15)) {
        errno = EINVAL; return -1;
    }
    process = OpenProcess(access, FALSE, (DWORD)pid);
    if (!process) return fx_win32_errno(GetLastError());
    if (!signal_number)
        rc = GetExitCodeProcess(process, &code) && code == STILL_ACTIVE ? 0 : -1;
    else rc = TerminateProcess(process, (UINT)(128 + signal_number)) ? 0 : -1;
    if (rc) fx_win32_errno(GetLastError());
    CloseHandle(process);
    return rc;
}

int fx_c_stderr_redirect(const char *path)
{
    wchar_t *wide = fx_win32_filename(path);
    int target, saved;
    if (!wide) return -1;
    fflush(stderr);
    target = _wopen(wide, _O_WRONLY | _O_CREAT | _O_TRUNC | _O_BINARY | _O_NOINHERIT,
                     _S_IREAD | _S_IWRITE);
    free(wide);
    if (target < 0) return -1;
    saved = _dup(2);
    if (saved < 0 || _dup2(target, 2) < 0) {
        _close(target); if (saved >= 0) _close(saved); return -1;
    }
    _close(target);
    return saved;
}

int fx_c_stderr_restore(int saved)
{
    int rc;
    if (saved < 0) return 0;
    fflush(stderr); rc = _dup2(saved, 2); _close(saved); fflush(stderr);
    return rc < 0 ? -1 : 0;
}

int fx_c_file_fingerprint(const char *path, long long *size, long long *mtime_ns)
{
    HANDLE file = fx_win32_file(path, FILE_READ_ATTRIBUTES, OPEN_EXISTING);
    BY_HANDLE_FILE_INFORMATION info;
    ULARGE_INTEGER value;
    int rc;
    if (size) *size = 0;
    if (mtime_ns) *mtime_ns = 0;
    if (file == INVALID_HANDLE_VALUE) return -1;
    rc = GetFileInformationByHandle(file, &info);
    if (!rc) fx_win32_errno(GetLastError());
    CloseHandle(file);
    if (!rc) return -1;
    value.HighPart = info.nFileSizeHigh; value.LowPart = info.nFileSizeLow;
    if (size) *size = (long long)value.QuadPart;
    value.HighPart = info.ftLastWriteTime.dwHighDateTime;
    value.LowPart = info.ftLastWriteTime.dwLowDateTime;
    if (mtime_ns) *mtime_ns = (long long)(value.QuadPart - 116444736000000000ULL) * 100;
    return 0;
}

int fx_c_file_stat(const char *path, long long *size, long long *mtime)
{
    long long nanos;
    int rc = fx_c_file_fingerprint(path, size, &nanos);
    if (mtime) *mtime = rc ? 0 : nanos / 1000000000LL;
    return rc;
}

long long fx_c_unix_time(void) { return (long long)time(NULL); }

int fx_c_set_mtime(const char *path, long long mtime)
{
    HANDLE file = fx_win32_file(path, FILE_WRITE_ATTRIBUTES, OPEN_EXISTING);
    ULARGE_INTEGER value;
    FILETIME stamp;
    int rc;
    if (file == INVALID_HANDLE_VALUE) return -1;
    value.QuadPart = (ULONGLONG)mtime * 10000000ULL + 116444736000000000ULL;
    stamp.dwHighDateTime = value.HighPart; stamp.dwLowDateTime = value.LowPart;
    rc = SetFileTime(file, NULL, &stamp, &stamp);
    if (!rc) fx_win32_errno(GetLastError());
    CloseHandle(file);
    return rc ? 0 : -1;
}

int fx_c_rename(const char *source, const char *destination)
{
    wchar_t *from = fx_win32_filename(source), *to;
    DWORD failure;
    int rc;
    if (!from) return -1;
    to = fx_win32_filename(destination);
    if (!to) { free(from); return -1; }
    rc = MoveFileExW(from, to, MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH);
    failure = GetLastError(); free(from); free(to);
    return rc ? 0 : fx_win32_errno(failure);
}

static int fx_win32_remove(const char *path, int directory)
{
    wchar_t *wide = fx_win32_filename(path);
    DWORD failure;
    int rc;
    if (!wide) return -1;
    rc = directory ? RemoveDirectoryW(wide) : DeleteFileW(wide);
    failure = GetLastError(); free(wide);
    if (rc || failure == ERROR_FILE_NOT_FOUND || failure == ERROR_PATH_NOT_FOUND) return 0;
    return fx_win32_errno(failure);
}

int fx_c_unlink(const char *path) { return fx_win32_remove(path, 0); }
int fx_c_rmdir(const char *path) { return fx_win32_remove(path, 1); }

int fx_c_path_is_dir(const char *path)
{
    wchar_t *wide = fx_win32_filename(path);
    DWORD attributes;
    if (!wide) return 0;
    attributes = GetFileAttributesW(wide); free(wide);
    return attributes != INVALID_FILE_ATTRIBUTES &&
           (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
}

int fx_c_mkdir_p(const char *path)
{
    wchar_t *wide = fx_win32_filename(path), *cursor;
    DWORD failure, attributes;
    int rc = 0;
    if (!wide) return -1;
    /* A drive root and a UNC share are namespace roots, never mkdir targets. */
    cursor = wide;
    if (!wcsncmp(wide, L"\\\\?\\UNC\\", 8)) {
        cursor += 8;
        while (*cursor && *cursor != L'\\') ++cursor;
        if (*cursor) ++cursor;
        while (*cursor && *cursor != L'\\') ++cursor;
        if (*cursor) ++cursor;
    } else if (!wcsncmp(wide, L"\\\\?\\", 4) && wcslen(wide) >= 7 &&
                wide[5] == L':') cursor += 7;
    else if (wcslen(wide) >= 3 && wide[1] == L':') cursor += 3;
    else if ((wide[0] == L'\\' || wide[0] == L'/') && wide[1] == wide[0]) {
        cursor += 2;
        while (*cursor && *cursor != L'\\' && *cursor != L'/') ++cursor;
        if (*cursor) ++cursor;
        while (*cursor && *cursor != L'\\' && *cursor != L'/') ++cursor;
        if (*cursor) ++cursor;
    } else if (*cursor == L'/' || *cursor == L'\\') ++cursor;
    for (;;) {
        wchar_t saved;
        while (*cursor && *cursor != L'\\' && *cursor != L'/') ++cursor;
        saved = *cursor; *cursor = 0;
        if (!CreateDirectoryW(wide, NULL)) {
            failure = GetLastError();
            attributes = GetFileAttributesW(wide);
            if (failure != ERROR_ALREADY_EXISTS || attributes == INVALID_FILE_ATTRIBUTES ||
                !(attributes & FILE_ATTRIBUTE_DIRECTORY)) {
                rc = fx_win32_errno(failure); break;
            }
        }
        *cursor = saved;
        if (!saved) break;
        ++cursor;
        if (!*cursor) break;
    }
    free(wide);
    return rc;
}

void fx_c_tmpfile(const char *prefix, char *path, int *length)
{
    static volatile LONG sequence = 0;
    wchar_t directory[32768], *wide;
    char *temporary = NULL, *base = NULL;
    HANDLE file;
    size_t extent;
    DWORD failure;
    int attempt;
    path[0] = 0; *length = 0;
    if (!prefix || !*prefix) {
        DWORD n = GetTempPathW(32768, directory);
        if (!n || n >= 32768) return;
        temporary = fx_win32_utf8(directory);
        if (!temporary) return;
        extent = strlen(temporary) + 4;
        base = malloc(extent);
        if (!base) { free(temporary); return; }
        snprintf(base, extent, "%sfx_", temporary); free(temporary);
        prefix = base;
    }
    extent = strlen(prefix) + 64;
    temporary = malloc(extent);
    if (!temporary) { free(base); return; }
    for (attempt = 0; attempt < 128; ++attempt) {
        snprintf(temporary, extent, "%s%lu-%llu-%lu", prefix,
                  (unsigned long)GetCurrentProcessId(),
                  (unsigned long long)GetTickCount64(),
                  (unsigned long)InterlockedIncrement(&sequence));
        if (strlen(temporary) >= 4096) { errno = ENAMETOOLONG; break; }
        wide = fx_win32_filename(temporary);
        if (!wide) break;
        file = CreateFileW(wide, GENERIC_READ | GENERIC_WRITE, 0, NULL,
                           CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
        failure = GetLastError(); free(wide);
        if (file != INVALID_HANDLE_VALUE) {
            CloseHandle(file); strcpy(path, temporary); *length = (int)strlen(path); break;
        }
        if (failure != ERROR_FILE_EXISTS && failure != ERROR_ALREADY_EXISTS) {
            fx_win32_errno(failure); break;
        }
    }
    free(temporary); free(base);
}

struct fx_win32_listing {
    char **paths;
    size_t count, capacity;
    int directories, scan;
    const char *extensions;
    int extension_count;
};

static void fx_win32_listing_free(struct fx_win32_listing *listing)
{
    size_t i;
    for (i = 0; i < listing->count; ++i) free(listing->paths[i]);
    free(listing->paths);
}

static int fx_win32_listing_add(struct fx_win32_listing *listing, const char *path)
{
    char **grown;
    size_t capacity;
    if (listing->count == listing->capacity) {
        capacity = listing->capacity ? listing->capacity * 2 : 64;
        if (capacity > SIZE_MAX / sizeof(*grown)) { errno = EOVERFLOW; return -1; }
        grown = realloc(listing->paths, capacity * sizeof(*grown));
        if (!grown) { errno = ENOMEM; return -1; }
        listing->paths = grown; listing->capacity = capacity;
    }
    listing->paths[listing->count] = strdup(path);
    if (!listing->paths[listing->count]) { errno = ENOMEM; return -1; }
    ++listing->count;
    return 0;
}

static int fx_win32_extension(const struct fx_win32_listing *listing, const char *path)
{
    const char *dot = strrchr(path, '.'), *cursor = listing->extensions;
    int i;
    if (!dot) return 0;
    for (i = 0; i < listing->extension_count; ++i) {
        if (!strcmp(dot, cursor)) return 1;
        cursor += strlen(cursor) + 1;
    }
    return 0;
}

static int fx_win32_list(const char *path, struct fx_win32_listing *listing, int root)
{
    wchar_t *wide = fx_win32_filename(path), *pattern;
    WIN32_FIND_DATAW data;
    HANDLE search;
    DWORD attributes, failure;
    char *name = NULL, *child = NULL;
    size_t length;
    int rc = 0, directory;
    if (!wide) return -1;
    attributes = GetFileAttributesW(wide);
    if (attributes == INVALID_FILE_ATTRIBUTES) {
        failure = GetLastError(); free(wide);
        if (listing->scan && root && (failure == ERROR_FILE_NOT_FOUND ||
                                      failure == ERROR_PATH_NOT_FOUND)) return 0;
        return fx_win32_errno(failure);
    }
    directory = (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
    if (directory && listing->directories)
        rc = fx_win32_listing_add(listing, path);
    else if (!directory && !listing->directories &&
              (!listing->scan || fx_win32_extension(listing, path)))
        rc = fx_win32_listing_add(listing, path);
    if (rc || !directory) { free(wide); return rc; }
    if (attributes & FILE_ATTRIBUTE_REPARSE_POINT) {
        free(wide);
        if (listing->scan) { errno = ENOTSUP; return -1; }
        return 0;
    }
    length = wcslen(wide);
    pattern = malloc((length + 3) * sizeof(*pattern));
    if (!pattern) { free(wide); errno = ENOMEM; return -1; }
    memcpy(pattern, wide, length * sizeof(*pattern)); free(wide);
    if (length && pattern[length - 1] != L'/' && pattern[length - 1] != L'\\')
        pattern[length++] = L'\\';
    pattern[length++] = L'*'; pattern[length] = 0;
    search = FindFirstFileW(pattern, &data); failure = GetLastError(); free(pattern);
    if (search == INVALID_HANDLE_VALUE)
        return failure == ERROR_FILE_NOT_FOUND ? 0 : fx_win32_errno(failure);
    for (;;) {
        if (wcscmp(data.cFileName, L".") && wcscmp(data.cFileName, L"..")) {
            name = fx_win32_utf8(data.cFileName);
            if (!name) { rc = fx_win32_errno(GetLastError()); break; }
            if (!(listing->scan && (!strcmp(name, ".git") || !strcmp(name, "build") ||
                                     !strcmp(name, "node_modules")))) {
                length = strlen(path) + strlen(name) + 2;
                child = malloc(length);
                if (!child) { free(name); errno = ENOMEM; rc = -1; break; }
                snprintf(child, length, "%s/%s", path, name);
                rc = fx_win32_list(child, listing, 0); free(child);
            }
            free(name);
            if (rc) break;
        }
        if (!FindNextFileW(search, &data)) {
            failure = GetLastError();
            if (failure != ERROR_NO_MORE_FILES) rc = fx_win32_errno(failure);
            break;
        }
    }
    FindClose(search);
    return rc;
}

static int fx_win32_path_compare(const void *left, const void *right)
{
    return strcmp(*(const char *const *)left, *(const char *const *)right);
}

static int fx_win32_collect(const char *root, int directories, const char *extensions,
                            int extension_count, char *slots, int width, int capacity,
                            int *count)
{
    struct fx_win32_listing listing = {0};
    size_t i;
    int rc;
    if (!count || !root || (slots && (width <= 0 || capacity < 0))) {
        errno = EINVAL; return -1;
    }
    *count = 0;
    listing.directories = directories; listing.scan = extensions != NULL;
    listing.extensions = extensions; listing.extension_count = extension_count;
    rc = fx_win32_list(root, &listing, 1);
    if (!rc && (listing.count > INT_MAX || (slots && listing.count > (size_t)capacity))) {
        errno = EOVERFLOW; rc = -1;
    }
    if (!rc && slots) {
        qsort(listing.paths, listing.count, sizeof(*listing.paths), fx_win32_path_compare);
        for (i = 0; i < listing.count; ++i) {
            if (strlen(listing.paths[i]) >= (size_t)width) {
                errno = ENAMETOOLONG; rc = -1; break;
            }
            memset(slots + i * width, 0, (size_t)width);
            strcpy(slots + i * width, listing.paths[i]);
        }
    }
    if (!rc) *count = (int)listing.count;
    fx_win32_listing_free(&listing);
    return rc;
}

int fx_c_scan_dir(const char *root, const char *extensions, int extension_count,
                  char *files, int *count, int capacity)
{
    if (!extensions || extension_count < 0 || !files) { errno = EINVAL; return -1; }
    return fx_win32_collect(root, 0, extensions, extension_count, files, 512, capacity, count);
}

int fx_c_count_dirs(const char *root, int *count)
{ return fx_win32_collect(root, 1, NULL, 0, NULL, 0, 0, count); }
int fx_c_count_files(const char *root, int *count)
{ return fx_win32_collect(root, 0, NULL, 0, NULL, 0, 0, count); }
int fx_c_collect_dirs(const char *root, char *slots, int width, int *count, int capacity)
{ return fx_win32_collect(root, 1, NULL, 0, slots, width, capacity, count); }
int fx_c_collect_files(const char *root, char *slots, int width, int *count, int capacity)
{ return fx_win32_collect(root, 0, NULL, 0, slots, width, capacity, count); }

/* Fo's native Windows provider owns recursive event/reconciliation semantics.
 * This older fd-based Fx interface has no Windows implementation. */
int fx_c_inotify_init(void) { errno = ENOTSUP; return -1; }
int fx_c_inotify_add_watch(int fd, const char *path, int mask)
{ (void)fd; (void)path; (void)mask; errno = ENOTSUP; return -1; }
int fx_c_inotify_rm_watch(int fd, int wd)
{ (void)fd; (void)wd; errno = ENOTSUP; return -1; }
int fx_c_inotify_close(int fd) { (void)fd; errno = ENOTSUP; return -1; }
int fx_c_inotify_poll(int fd, char *path, int length, int *kind, int timeout)
{ (void)fd; (void)path; (void)length; (void)kind; (void)timeout; errno = ENOTSUP; return -1; }
