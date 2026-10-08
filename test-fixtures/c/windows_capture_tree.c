/* An independently retained native process handle is the drainage oracle. */
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

int fx_c_exec(const char *, int, char *, int *, char *, int *);
int fx_c_exec_silent(const char *, int);

struct capture {
    char packed[65536], out[128], err[128];
    int silent, result, out_length, err_length;
};

static DWORD WINAPI run_capture(void *opaque)
{
    struct capture *capture = opaque;
    capture->out_length = sizeof(capture->out);
    capture->err_length = sizeof(capture->err);
    capture->result = capture->silent ? fx_c_exec_silent(capture->packed, 3) :
        fx_c_exec(capture->packed, 3, capture->out, &capture->out_length,
                  capture->err, &capture->err_length);
    return 0;
}

static int start_native(wchar_t *image, const wchar_t *mode, BOOL inherit,
                         PROCESS_INFORMATION *process)
{
    wchar_t command[32768];
    STARTUPINFOW startup = {0};
    startup.cb = sizeof(startup);
    if (swprintf(command, 32768, L"\"%ls\" %ls", image, mode) < 0) return 0;
    return CreateProcessW(NULL, command, NULL, NULL, inherit, 0, NULL, NULL,
                           &startup, process);
}

static int leader(wchar_t *image, wchar_t *name)
{
    wchar_t event_name[256];
    HANDLE mapping = OpenFileMappingW(FILE_MAP_WRITE, FALSE, name), event;
    volatile LONG *pid;
    PROCESS_INFORMATION child = {0};
    DWORD waited;
    if (!mapping) return 2;
    pid = MapViewOfFile(mapping, FILE_MAP_WRITE, 0, 0, sizeof(*pid));
    swprintf(event_name, 256, L"%ls-held", name);
    event = OpenEventW(SYNCHRONIZE, FALSE, event_name);
    if (!pid || !event || !start_native(image, L"--leaf", TRUE, &child)) return 3;
    CloseHandle(child.hThread);
    InterlockedExchange(pid, (LONG)child.dwProcessId);
    waited = WaitForSingleObject(event, 10000);
    CloseHandle(child.hProcess); CloseHandle(event);
    UnmapViewOfFile((void *)pid); CloseHandle(mapping);
    return waited == WAIT_OBJECT_0 ? 42 : 4;
}

static int check_capture(wchar_t *image, int silent)
{
    wchar_t name[128], event_name[256];
    char image_utf8[65536], name_utf8[256];
    HANDLE mapping, event, thread, descendant = NULL;
    volatile LONG *pid;
    FILETIME birth, exit_time, kernel, user;
    ULONGLONG deadline = GetTickCount64() + 10000;
    struct capture capture = {0};
    int result = 1;
    DWORD state;
    size_t length;
    swprintf(name, 128, L"Local\\fx-capture-%lu-%d", GetCurrentProcessId(), silent);
    swprintf(event_name, 256, L"%ls-held", name);
    mapping = CreateFileMappingW(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
                                  0, sizeof(LONG), name);
    if (!mapping || GetLastError() == ERROR_ALREADY_EXISTS) return 1;
    pid = MapViewOfFile(mapping, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(*pid));
    event = CreateEventW(NULL, TRUE, FALSE, event_name);
    if (!pid || !event) return 1;
    InterlockedExchange(pid, 0);
    if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, image, -1,
             image_utf8, sizeof(image_utf8), NULL, NULL) ||
        !WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, name, -1,
             name_utf8, sizeof(name_utf8), NULL, NULL)) return 1;
    length = strlen(image_utf8) + 1;
    memcpy(capture.packed, image_utf8, length);
    memcpy(capture.packed + length, "--leader", 9); length += 9;
    memcpy(capture.packed + length, name_utf8, strlen(name_utf8) + 1);
    capture.silent = silent;
    thread = CreateThread(NULL, 0, run_capture, &capture, 0, NULL);
    if (!thread) return 1;
    while (!*pid && GetTickCount64() < deadline) {
        if (WaitForSingleObject(thread, 0) == WAIT_OBJECT_0) break;
        Sleep(1);
    }
    if (*pid > 0) descendant = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION |
                                            SYNCHRONIZE | PROCESS_TERMINATE,
                                            FALSE, (DWORD)*pid);
    if (descendant && GetProcessTimes(descendant, &birth, &exit_time, &kernel, &user)) {
        SetEvent(event);
        state = WaitForSingleObject(thread, 10000);
        result = state != WAIT_OBJECT_0 || capture.result != 42 ||
                  WaitForSingleObject(descendant, 0) != WAIT_OBJECT_0;
        printf("%s: %s capture returns exit42 after exact descendant signals\n",
                result ? "FAIL" : "PASS", silent ? "silent" : "piped");
    } else SetEvent(event);
    if (descendant) {
        if (WaitForSingleObject(descendant, 0) != WAIT_OBJECT_0) {
            TerminateProcess(descendant, 1); WaitForSingleObject(descendant, 5000);
        }
        CloseHandle(descendant);
    }
    WaitForSingleObject(thread, 10000); CloseHandle(thread);
    CloseHandle(event); UnmapViewOfFile((void *)pid); CloseHandle(mapping);
    return result;
}

int wmain(int argc, wchar_t **argv)
{
    wchar_t image[32768];
    PROCESS_INFORMATION peer = {0};
    int failures;
    if (!GetModuleFileNameW(NULL, image, 32768)) return 2;
    if (argc > 1 && !wcscmp(argv[1], L"--leaf")) {
        Sleep(INFINITE); return 1;
    }
    if (argc == 3 && !wcscmp(argv[1], L"--leader")) return leader(image, argv[2]);
    if (!start_native(image, L"--leaf", FALSE, &peer)) return 2;
    CloseHandle(peer.hThread);
    failures = check_capture(image, 0) + check_capture(image, 1);
    if (WaitForSingleObject(peer.hProcess, 0) != WAIT_TIMEOUT) {
        puts("FAIL: independent unrelated peer was disturbed"); ++failures;
    } else puts("PASS: independent unrelated peer remains alive");
    TerminateProcess(peer.hProcess, 0); WaitForSingleObject(peer.hProcess, 5000);
    CloseHandle(peer.hProcess);
    printf("windows-capture-tree: %d failures\n", failures);
    return failures ? 1 : 0;
}
