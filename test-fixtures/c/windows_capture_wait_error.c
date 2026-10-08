/* Inject one Win32 wait error; real native handles prove error-path cleanup. */
#include <windows.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>

static HANDLE observed;
static int inject_error;
static DWORD WINAPI wait_with_error(HANDLE handle, DWORD timeout)
{
    if (inject_error) {
        inject_error = 0;
        if (!DuplicateHandle(GetCurrentProcess(), handle, GetCurrentProcess(),
                              &observed, SYNCHRONIZE, FALSE, 0)) abort();
        SetLastError(ERROR_INVALID_HANDLE);
        return WAIT_FAILED;
    }
    return WaitForSingleObject(handle, timeout);
}
#define WaitForSingleObject wait_with_error
#include "../../src/proc/fx_sys.c"
#undef WaitForSingleObject

int wmain(int argc, wchar_t **argv)
{
    wchar_t image[32768];
    char utf8[65536], packed[65536], out[32], err[32];
    int mode, failures = 0, result, error, out_len, err_len;
    size_t length;
    if (argc > 1 && !wcscmp(argv[1], L"--leaf")) { Sleep(INFINITE); return 1; }
    if (!GetModuleFileNameW(NULL, image, 32768) ||
        !WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, image, -1,
                             utf8, sizeof(utf8), NULL, NULL)) return 2;
    length = strlen(utf8) + 1;
    memcpy(packed, utf8, length); memcpy(packed + length, "--leaf", 7);
    for (mode = 0; mode < 2; ++mode) {
        observed = NULL; inject_error = 1; errno = 0;
        out_len = sizeof(out); err_len = sizeof(err);
        result = mode ? fx_c_exec_silent(packed, 2) :
            fx_c_exec(packed, 2, out, &out_len, err, &err_len);
        error = errno;
        if (result != -1 || error != EBADF || !observed ||
            WaitForSingleObject(observed, 0) != WAIT_OBJECT_0) {
            printf("FAIL: %s wait error result=%d errno=%d leaves no owned child\n",
                    mode ? "silent" : "piped", result, error); ++failures;
        } else printf("PASS: %s wait error returns EBADF after exact child signals\n",
                        mode ? "silent" : "piped");
        if (observed) CloseHandle(observed);
    }
    printf("windows-capture-wait-error: %d failures\n", failures);
    return failures ? 1 : 0;
}
