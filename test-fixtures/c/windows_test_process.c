/* Native behavioral oracle for the independent Fx fixture process boundary. */
#define _WIN32_WINNT 0x0a00
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <errno.h>
#include <io.h>
#include <fcntl.h>

extern wchar_t *fx_win32_command_line(const char *, int, int);
extern wchar_t *fx_win32_utf16(const char *);
extern int fx_test_process_spawn(const char *, const int *, int, int *);
extern int fx_test_process_spawn_piped(const char *, const int *, int, int, int *, int *, int *);
extern int fx_test_process_pipe_read(int, char *, int, int);
extern int fx_test_process_pipe_write(int, const char *, int);
extern int fx_test_process_close_fd(int);
extern int fx_test_process_is_executable(const char *);
extern int fx_test_process_wait_once(int, int *);
extern int fx_test_process_signal(int, int);
extern int fx_test_process_identity(int, int64_t *, int *, char *, int);
extern int64_t fx_test_process_clock_ms(void);
extern void fx_test_process_sleep_ms(int);

static int failures;
static void check(int okay, const char *message) {
    printf("%s: %s\n", okay ? "PASS" : "FAIL", message); fflush(stdout);
    if (!okay) ++failures;
}
static int pack(char *bytes, int *offsets, const char **argv, int count) {
    int used = 0;
    for (int index = 0; index < count; ++index) {
        int size = (int)strlen(argv[index]) + 1;
        offsets[index] = used + 1; memcpy(bytes + used, argv[index], (size_t)size); used += size;
    }
    return used;
}
static PROCESS_INFORMATION bare_spawn(char *bytes, int size, int count) {
    wchar_t *command = fx_win32_command_line(bytes, size, count);
    STARTUPINFOW startup = {0}; startup.cb = sizeof(startup);
    PROCESS_INFORMATION child = {0};
    check(command && CreateProcessW(NULL, command, NULL, NULL, FALSE, 0, NULL, NULL, &startup, &child),
        "independent native control launches without fixture APIs");
    free(command); if (child.hThread) CloseHandle(child.hThread); return child;
}
static uint64_t birth(HANDLE handle) {
    FILETIME created, exited, kernel, user;
    if (!GetProcessTimes(handle, &created, &exited, &kernel, &user)) return 0;
    return ((uint64_t)created.dwHighDateTime << 32) | created.dwLowDateTime;
}
static int same_image(const wchar_t *left, const char *right) {
    wchar_t *wide = fx_win32_utf16(right);
    HANDLE a = CreateFileW(left, FILE_READ_ATTRIBUTES, FILE_SHARE_READ | FILE_SHARE_WRITE |
        FILE_SHARE_DELETE, NULL, OPEN_EXISTING, 0, NULL);
    HANDLE b = wide ? CreateFileW(wide, FILE_READ_ATTRIBUTES, FILE_SHARE_READ | FILE_SHARE_WRITE |
        FILE_SHARE_DELETE, NULL, OPEN_EXISTING, 0, NULL) : INVALID_HANDLE_VALUE;
    BY_HANDLE_FILE_INFORMATION x, y;
    int same = a != INVALID_HANDLE_VALUE && b != INVALID_HANDLE_VALUE &&
        GetFileInformationByHandle(a, &x) && GetFileInformationByHandle(b, &y) &&
        x.dwVolumeSerialNumber == y.dwVolumeSerialNumber && x.nFileIndexHigh == y.nFileIndexHigh &&
        x.nFileIndexLow == y.nFileIndexLow;
    if (a != INVALID_HANDLE_VALUE) CloseHandle(a);
    if (b != INVALID_HANDLE_VALUE) CloseHandle(b);
    free(wide); return same;
}
static int wait_child(int pid, int *status) {
    ULONGLONG deadline = GetTickCount64() + 5000;
    int done;
    do { done = fx_test_process_wait_once(pid, status); if (!done) Sleep(1); }
    while (!done && GetTickCount64() < deadline);
    return done;
}
int wmain(int argc, wchar_t **argv) {
    if (argc > 1 && !wcscmp(argv[1], L"--exit")) return 42;
    if (argc > 1 && !wcscmp(argv[1], L"--idle")) { for (;;) Sleep(10); }
    if (argc > 1 && !wcscmp(argv[1], L"--echo")) {
        int okay = argc == 6 && !wcscmp(argv[2], L"caf\u00e9-\U0001f680") &&
            !wcscmp(argv[3], L"") && !wcscmp(argv[4], L"space \"quote") && !wcscmp(argv[5], L"tail\\");
        const char expected[5] = {'a', 0, (char)0xff, '\r', '\n'};
        char bytes[5]; DWORD received = 0, written = 0;
        okay = okay && ReadFile(GetStdHandle(STD_INPUT_HANDLE), bytes, sizeof(bytes), &received, NULL) &&
            received == sizeof(bytes) && !memcmp(bytes, expected, sizeof(bytes));
        if (!okay) return 83;
        WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), "out:", 4, &written, NULL);
        WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), bytes, sizeof(bytes), &written, NULL);
        WriteFile(GetStdHandle(STD_ERROR_HANDLE), "err:", 4, &written, NULL);
        WriteFile(GetStdHandle(STD_ERROR_HANDLE), bytes, sizeof(bytes), &written, NULL);
        return 42;
    }
    wchar_t wide_image[32768]; char image[65536], bytes[131072]; int offsets[8];
    if (!GetModuleFileNameW(NULL, wide_image, 32768) || !WideCharToMultiByte(CP_UTF8,
        WC_ERR_INVALID_CHARS, wide_image, -1, image, sizeof(image), NULL, NULL)) return 90;
    const char *exit_args[] = {image, "--exit"};
    int size = pack(bytes, offsets, exit_args, 2);
    PROCESS_INFORMATION warm = bare_spawn(bytes, size, 2);
    check(warm.hProcess && WaitForSingleObject(warm.hProcess, 5000) == WAIT_OBJECT_0,
        "bare CreateProcessW initialization finishes before handle baseline");
    if (warm.hProcess) CloseHandle(warm.hProcess);
    for (int index = 0; index < 3; ++index) {
        int pipe[2]; char byte;
        if (_pipe(pipe, 4096, _O_BINARY | _O_NOINHERIT)) return 91;
        _write(pipe[1], "x", 1); _read(pipe[0], &byte, 1); _close(pipe[0]); _close(pipe[1]);
    }
    DWORD before = 0, after = 0; GetProcessHandleCount(GetCurrentProcess(), &before);
    check(fx_test_process_is_executable(image) && !fx_test_process_is_executable("C:\\Windows"),
        "executable probe recognizes native PE and rejects directories");
    const char *idle_args[] = {image, "--idle"};
    size = pack(bytes, offsets, idle_args, 2);
    PROCESS_INFORMATION peer = bare_spawn(bytes, size, 2);
    int pid = 0, status = 0;
    check(!fx_test_process_spawn(bytes, offsets, 2, &pid), "launches direct native fixture child");
    int64_t start = 0; int parent = 0; char path[65536];
    check(!fx_test_process_identity(pid, &start, &parent, path, sizeof(path)),
        "identity uses native process creation time, parent and image path");
    HANDLE child = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, (DWORD)pid);
    check(child && (uint64_t)start == birth(child) && parent == (int)GetCurrentProcessId() &&
        same_image(wide_image, path), "independent held HANDLE confirms exact birth, direct parent and actual executable");
    check(!fx_test_process_signal(pid, 0), "zero signal checks the exact live owned child");
    check(fx_test_process_signal((int)peer.dwProcessId, 9) == ESRCH &&
        WaitForSingleObject(peer.hProcess, 0) == WAIT_TIMEOUT, "numeric peer PID carries no fixture kill authority");
    check(!fx_test_process_signal(pid, 9) && wait_child(pid, &status) == 1 && status == 137,
        "hard stop targets exact retained child and reports actual native termination exit");
    check(child && WaitForSingleObject(child, 0) == WAIT_OBJECT_0, "exact held child HANDLE is signaled at reap");
    if (child) CloseHandle(child);
    check(fx_test_process_wait_once(pid, &status) == -1 && fx_test_process_signal(pid, 9) == ESRCH,
        "reaped numeric PID cannot be waited or signaled again");
    const char *echo_args[] = {image, "--echo", "caf\xc3\xa9-\xf0\x9f\x9a\x80", "", "space \"quote", "tail\\"};
    pack(bytes, offsets, echo_args, 6);
    int input = -1, output = -1;
    check(!fx_test_process_spawn_piped(bytes, offsets, 6, 1, &pid, &input, &output),
        "starts native piped producer with exact UTF8 CRT argv");
    int64_t began = fx_test_process_clock_ms();
    check(fx_test_process_pipe_read(output, bytes, 32, 50) == -2 &&
        fx_test_process_clock_ms() - began >= 35 && fx_test_process_clock_ms() - began < 1000,
        "empty pipe returns bounded timeout while child remains alive");
    const char token[5] = {'a', 0, (char)0xff, '\r', '\n'};
    check(fx_test_process_pipe_write(input, token, 5) == 5, "writes exact binary bytes without newline conversion");
    fx_test_process_close_fd(input);
    char received[64] = {0}; int count = 0, got = 0;
    do { got = fx_test_process_pipe_read(output, received + count, sizeof(received) - count, 1000);
        if (got > 0) count += got; } while (got > 0 && count < (int)sizeof(received));
    check(count == 18 && !memcmp(received, "out:", 4) && !memcmp(received + 4, token, 5) &&
        !memcmp(received + 9, "err:", 4) && !memcmp(received + 13, token, 5) && got == 0,
        "piped stdout and requested diagnostics retain exact binary bytes and real EOF");
    fx_test_process_close_fd(output);
    check(wait_child(pid, &status) == 1 && status == 42, "piped wait retains real completed exit status");
    check(TerminateProcess(peer.hProcess, 0) && WaitForSingleObject(peer.hProcess, 5000) == WAIT_OBJECT_0,
        "independent control peer is cleaned only through its held HANDLE");
    CloseHandle(peer.hProcess);
    GetProcessHandleCount(GetCurrentProcess(), &after);
    printf("native-fixture-handles: before=%lu after=%lu\n", (unsigned long)before, (unsigned long)after);
    check(before == after, "all fixture process, thread and pipe handles return exactly to baseline");
    return failures ? 1 : 0;
}
