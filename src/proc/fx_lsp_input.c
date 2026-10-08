/* Raw, bounded stdin transport: formatted Fortran input may read ahead. */
#include <errno.h>
#include <stdio.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#else
#include <poll.h>
#include <unistd.h>
#endif

/* Positive byte count, zero timeout, -1 EOF, -2 transport error. */
int fx_lsp_input(char *bytes, int capacity, int timeout_ms)
{
#if defined(_WIN32) && !defined(__CYGWIN__)
    HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
    DWORD available, count;
    ULONGLONG start = GetTickCount64();
    _setmode(_fileno(stdin), _O_BINARY);
    if (GetFileType(input) == FILE_TYPE_PIPE) {
        for (;;) {
            if (!PeekNamedPipe(input, NULL, 0, NULL, &available, NULL))
                return GetLastError() == ERROR_BROKEN_PIPE ? -1 : -2;
            if (available > 0) break;
            if (timeout_ms >= 0 && GetTickCount64() - start >= (DWORD)timeout_ms)
                return 0;
            Sleep(1);
        }
    }
    if (!ReadFile(input, bytes, (DWORD)capacity, &count, NULL))
        return GetLastError() == ERROR_BROKEN_PIPE ? -1 : -2;
    return count ? (int)count : -1;
#else
    struct pollfd descriptor = {STDIN_FILENO, POLLIN, 0};
    int ready;
    ssize_t count;
    do { ready = poll(&descriptor, 1, timeout_ms); }
    while (ready < 0 && errno == EINTR);
    if (ready == 0) return 0;
    if (ready < 0) return -2;
    do { count = read(STDIN_FILENO, bytes, (size_t)capacity); }
    while (count < 0 && errno == EINTR);
    return count > 0 ? (int)count : count == 0 ? -1 : -2;
#endif
}
