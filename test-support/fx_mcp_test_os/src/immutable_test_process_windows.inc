/* Independent native test producers retain their own exact root handles.
   They inherit existing containment and never add production supervision. */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0a00
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tlhelp32.h>
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <limits.h>
#include <io.h>
#include <fcntl.h>

extern wchar_t *fx_win32_utf16(const char *);
extern wchar_t *fx_win32_command_line(const char *, int, int);
struct test_child { DWORD pid; uint64_t birth; HANDLE handle; struct test_child *next; };
static struct test_child *test_children;
static SRWLOCK test_child_lock = SRWLOCK_INIT;

static int test_error(DWORD error) {
    switch (error) {
    case ERROR_FILE_NOT_FOUND: case ERROR_PATH_NOT_FOUND: return ENOENT;
    case ERROR_ACCESS_DENIED: return EACCES;
    case ERROR_NOT_ENOUGH_MEMORY: case ERROR_OUTOFMEMORY: return ENOMEM;
    case ERROR_INVALID_PARAMETER: return EINVAL;
    case ERROR_INVALID_HANDLE: return EBADF;
    case ERROR_INSUFFICIENT_BUFFER: return ENAMETOOLONG;
    case ERROR_BROKEN_PIPE: case ERROR_NO_DATA: return EPIPE;
    default: return EIO;
    }
}
static uint64_t test_birth(HANDLE process) {
    FILETIME created, exited, kernel, user;
    if (!GetProcessTimes(process, &created, &exited, &kernel, &user)) return 0;
    return ((uint64_t)created.dwHighDateTime << 32) | created.dwLowDateTime;
}
static HANDLE test_standard_handle(DWORD standard, int writing) {
    HANDLE original = GetStdHandle(standard), copy = NULL;
    if (original && original != INVALID_HANDLE_VALUE &&
        DuplicateHandle(GetCurrentProcess(), original, GetCurrentProcess(), &copy,
            0, TRUE, DUPLICATE_SAME_ACCESS)) return copy;
    SECURITY_ATTRIBUTES security = {sizeof(security), NULL, TRUE};
    copy = CreateFileW(L"NUL", writing ? GENERIC_WRITE : GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, &security, OPEN_EXISTING, 0, NULL);
    return copy == INVALID_HANDLE_VALUE ? NULL : copy;
}
static int test_spawn(const char *bytes, const int *offsets, int count,
    const int descriptors[2], int capture_stderr, int *child_pid) {
    if (!bytes || !offsets || count < 1 || !child_pid) return EINVAL;
    *child_pid = 0;
    size_t length = 0;
    for (int index = 0; index < count; ++index) {
        if (offsets[index] < 1) return EINVAL;
        size_t used = strlen(bytes + offsets[index] - 1) + 1;
        if (used > INT_MAX || length > INT_MAX - used) return EOVERFLOW;
        length += used;
    }
    char *packed = malloc(length);
    struct test_child *child = calloc(1, sizeof(*child));
    if (!packed || !child) { free(packed); free(child); return ENOMEM; }
    size_t position = 0;
    for (int index = 0; index < count; ++index) {
        size_t used = strlen(bytes + offsets[index] - 1) + 1;
        memcpy(packed + position, bytes + offsets[index] - 1, used); position += used;
    }
    wchar_t *command = fx_win32_command_line(packed, (int)length, count);
    free(packed);
    if (!command) { free(child); return errno; }
    STARTUPINFOEXW startup = {0}; PROCESS_INFORMATION process = {0};
    startup.StartupInfo.cb = sizeof(startup);
    HANDLE inherited[3] = {NULL}; SIZE_T extent = 0;
    int error = 0, initialized = 0, streams = 0;
    for (int index = 0; index < 3; ++index) {
        if (descriptors && (index != 2 || capture_stderr)) {
            intptr_t original = _get_osfhandle(descriptors[index == 0 ? 0 : 1]);
            if (original == -1 || !DuplicateHandle(GetCurrentProcess(), (HANDLE)original,
                GetCurrentProcess(), &inherited[index], 0, TRUE, DUPLICATE_SAME_ACCESS)) {
                error = original == -1 ? EBADF : test_error(GetLastError()); goto done;
            }
        } else inherited[index] = test_standard_handle(index == 0 ? STD_INPUT_HANDLE :
            (index == 1 ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE), index != 0);
        if (!inherited[index]) { error = test_error(GetLastError()); goto done; }
        ++streams;
    }
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = inherited[0];
    startup.StartupInfo.hStdOutput = inherited[1]; startup.StartupInfo.hStdError = inherited[2];
    InitializeProcThreadAttributeList(NULL, 1, 0, &extent);
    startup.lpAttributeList = malloc(extent);
    if (!startup.lpAttributeList) { error = ENOMEM; goto done; }
    if (!InitializeProcThreadAttributeList(startup.lpAttributeList, 1, 0, &extent)) {
        error = test_error(GetLastError()); goto done;
    }
    initialized = 1;
    if (!UpdateProcThreadAttribute(startup.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
        inherited, sizeof(inherited), NULL, NULL) ||
        !CreateProcessW(NULL, command, NULL, NULL, TRUE, EXTENDED_STARTUPINFO_PRESENT,
            NULL, NULL, &startup.StartupInfo, &process)) { error = test_error(GetLastError()); goto done; }
    child->pid = process.dwProcessId; child->handle = process.hProcess;
    child->birth = test_birth(child->handle);
    if (!child->birth || child->pid > INT_MAX) {
        error = EIO; TerminateProcess(child->handle, 126); WaitForSingleObject(child->handle, 5000);
        CloseHandle(child->handle); goto done;
    }
    AcquireSRWLockExclusive(&test_child_lock);
    child->next = test_children; test_children = child;
    *child_pid = (int)child->pid; child = NULL;
    ReleaseSRWLockExclusive(&test_child_lock);
done:
    if (process.hThread) CloseHandle(process.hThread);
    if (initialized) DeleteProcThreadAttributeList(startup.lpAttributeList);
    free(startup.lpAttributeList); free(command); free(child);
    for (int index = 0; index < streams; ++index) CloseHandle(inherited[index]);
    return error;
}
int fx_test_process_spawn(const char *bytes, const int *offsets, int count, int *pid) {
    return test_spawn(bytes, offsets, count, NULL, 0, pid);
}
int fx_test_process_spawn_piped(const char *bytes, const int *offsets, int count,
    int capture_stderr, int *pid, int *input_fd, int *output_fd) {
    int input[2], output[2];
    *input_fd = -1; *output_fd = -1;
    if (_pipe(input, 4096, _O_BINARY | _O_NOINHERIT)) return errno;
    if (_pipe(output, 4096, _O_BINARY | _O_NOINHERIT)) {
        int error = errno; _close(input[0]); _close(input[1]); return error;
    }
    int descriptors[2] = {input[0], output[1]};
    int error = test_spawn(bytes, offsets, count, descriptors, capture_stderr, pid);
    _close(input[0]); _close(output[1]);
    if (error) { _close(input[1]); _close(output[0]); return error; }
    *input_fd = input[1]; *output_fd = output[0]; return 0;
}
int fx_test_process_pipe_read(int fd, char *bytes, int capacity, int timeout_ms) {
    if (fd < 0 || !bytes || capacity < 1) { errno = EINVAL; return -1; }
    HANDLE pipe = (HANDLE)_get_osfhandle(fd);
    if (pipe == INVALID_HANDLE_VALUE) { errno = EBADF; return -1; }
    ULONGLONG deadline = GetTickCount64() + (timeout_ms < 0 ? 0 : (DWORD)timeout_ms);
    for (;;) {
        DWORD available = 0, received = 0;
        if (!PeekNamedPipe(pipe, NULL, 0, NULL, &available, NULL)) {
            DWORD failure = GetLastError();
            if (failure == ERROR_BROKEN_PIPE) return 0;
            errno = test_error(failure); return -1;
        }
        if (available) {
            DWORD count = available < (DWORD)capacity ? available : (DWORD)capacity;
            if (!ReadFile(pipe, bytes, count, &received, NULL)) {
                errno = test_error(GetLastError()); return -1;
            }
            return (int)received;
        }
        if (timeout_ms >= 0 && GetTickCount64() >= deadline) return -2;
        Sleep(1);
    }
}
int fx_test_process_pipe_write(int fd, const char *bytes, int count) {
    if (fd < 0 || count < 0 || (!bytes && count)) { errno = EINVAL; return -1; }
    HANDLE pipe = (HANDLE)_get_osfhandle(fd);
    if (pipe == INVALID_HANDLE_VALUE) { errno = EBADF; return -1; }
    int done = 0;
    while (done < count) {
        DWORD written = 0;
        if (!WriteFile(pipe, bytes + done, (DWORD)(count - done), &written, NULL) || !written) {
            errno = test_error(GetLastError()); return -1;
        }
        done += (int)written;
    }
    return done;
}
int fx_test_process_close_fd(int fd) { return fd < 0 ? 0 : _close(fd); }
int fx_test_process_is_executable(const char *path) {
    wchar_t *wide = path ? fx_win32_utf16(path) : NULL;
    DWORD type = 0;
    int executable = wide && GetBinaryTypeW(wide, &type);
    free(wide);
    return executable;
}
int fx_test_process_wait_once(int pid, int *status) {
    if (pid <= 0 || !status) { errno = EINVAL; return -1; }
    AcquireSRWLockExclusive(&test_child_lock);
    struct test_child **slot = &test_children;
    while (*slot && (*slot)->pid != (DWORD)pid) slot = &(*slot)->next;
    struct test_child *child = *slot;
    if (!child) { ReleaseSRWLockExclusive(&test_child_lock); errno = ECHILD; return -1; }
    DWORD wait = WaitForSingleObject(child->handle, 0), code = 0;
    int result = 0;
    if (wait == WAIT_OBJECT_0 && GetExitCodeProcess(child->handle, &code)) {
        *status = (int)code; *slot = child->next;
        CloseHandle(child->handle); free(child); result = 1;
    } else if (wait == WAIT_FAILED || wait == WAIT_OBJECT_0) {
        errno = test_error(GetLastError()); result = -1;
    }
    ReleaseSRWLockExclusive(&test_child_lock); return result;
}
int fx_test_process_signal(int pid, int signal_number) {
    if (pid <= 0 || (signal_number != 0 && signal_number != 9)) return EINVAL;
    AcquireSRWLockExclusive(&test_child_lock);
    struct test_child *child = test_children;
    while (child && child->pid != (DWORD)pid) child = child->next;
    int error = ESRCH;
    if (child && test_birth(child->handle) == child->birth &&
        WaitForSingleObject(child->handle, 0) == WAIT_TIMEOUT)
        error = signal_number == 0 || TerminateProcess(child->handle, 137) ? 0 : test_error(GetLastError());
    ReleaseSRWLockExclusive(&test_child_lock); return error;
}
int fx_test_process_identity(int pid, int64_t *start, int *parent, char *path, int capacity) {
    if (pid <= 0 || !start || !parent || !path || capacity < 1) return EINVAL;
    path[0] = 0;
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, (DWORD)pid);
    if (!process) return test_error(GetLastError());
    uint64_t birth = test_birth(process);
    int error = !birth || WaitForSingleObject(process, 0) != WAIT_TIMEOUT ? ESRCH : 0;
    wchar_t *wide = malloc(32768 * sizeof(*wide)); DWORD used = 32768;
    if (!wide && !error) error = ENOMEM;
    if (!error && !QueryFullProcessImageNameW(process, 0, wide, &used)) error = test_error(GetLastError());
    if (!error && !WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1,
        path, capacity, NULL, NULL)) error = test_error(GetLastError());
    free(wide);
    HANDLE snapshot = error ? INVALID_HANDLE_VALUE : CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE && !error) error = test_error(GetLastError());
    if (!error) {
        PROCESSENTRY32W entry = {0}; entry.dwSize = sizeof(entry);
        int found = 0;
        if (Process32FirstW(snapshot, &entry)) do {
            if (entry.th32ProcessID == (DWORD)pid) {
                *parent = (int)entry.th32ParentProcessID; found = 1; break;
            }
        } while (Process32NextW(snapshot, &entry));
        if (!found || WaitForSingleObject(process, 0) != WAIT_TIMEOUT || test_birth(process) != birth)
            error = ESRCH;
    }
    if (snapshot != INVALID_HANDLE_VALUE) CloseHandle(snapshot);
    CloseHandle(process);
    if (!error) *start = (int64_t)birth;
    else path[0] = 0;
    return error;
}
int64_t fx_test_process_clock_ms(void) { return (int64_t)GetTickCount64(); }
void fx_test_process_sleep_ms(int milliseconds) { if (milliseconds > 0) Sleep((DWORD)milliseconds); }
