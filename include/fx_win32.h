/* Internal UTF-8 boundary shared by native Win32 system interfaces. */
#ifndef FX_WIN32_H
#define FX_WIN32_H
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <errno.h>
#include <stdlib.h>
#include <wchar.h>

static inline wchar_t *fx_win32_wide(const char *text)
{
    int count;
    wchar_t *wide;
    if (!text) { SetLastError(ERROR_INVALID_PARAMETER); return NULL; }
    count = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, -1, NULL, 0);
    if (!count) return NULL;
    wide = malloc((size_t)count * sizeof(*wide));
    if (!wide) { SetLastError(ERROR_NOT_ENOUGH_MEMORY); return NULL; }
    if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, -1, wide, count)) {
        DWORD error = GetLastError(); free(wide); SetLastError(error); return NULL;
    }
    return wide;
}

static inline char *fx_win32_utf8(const wchar_t *wide)
{
    int count;
    char *text;
    if (!wide) { SetLastError(ERROR_INVALID_PARAMETER); return NULL; }
    count = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1,
                                NULL, 0, NULL, NULL);
    if (!count) return NULL;
    text = malloc((size_t)count);
    if (!text) { SetLastError(ERROR_NOT_ENOUGH_MEMORY); return NULL; }
    if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, -1,
                            text, count, NULL, NULL)) {
        DWORD error = GetLastError(); free(text); SetLastError(error); return NULL;
    }
    return text;
}

static inline int fx_win32_errno(DWORD error)
{
    switch (error) {
    case ERROR_FILE_NOT_FOUND: case ERROR_PATH_NOT_FOUND: errno = ENOENT; break;
    case ERROR_ALREADY_EXISTS: case ERROR_FILE_EXISTS: errno = EEXIST; break;
    case ERROR_ACCESS_DENIED: case ERROR_PRIVILEGE_NOT_HELD: errno = EACCES; break;
    case ERROR_SHARING_VIOLATION: case ERROR_LOCK_VIOLATION: errno = EBUSY; break;
    case ERROR_INVALID_HANDLE: errno = EBADF; break;
    case ERROR_NOT_ENOUGH_MEMORY: case ERROR_OUTOFMEMORY: errno = ENOMEM; break;
    case ERROR_DIRECTORY: errno = ENOTDIR; break;
    case ERROR_DIR_NOT_EMPTY: errno = ENOTEMPTY; break;
    case ERROR_DISK_FULL: errno = ENOSPC; break;
    case ERROR_FILENAME_EXCED_RANGE: errno = ENAMETOOLONG; break;
    case ERROR_NOT_SAME_DEVICE: errno = EXDEV; break;
    case ERROR_NOT_SUPPORTED: case ERROR_INVALID_FUNCTION: errno = ENOTSUP; break;
    case ERROR_NO_UNICODE_TRANSLATION: case ERROR_INVALID_PARAMETER:
        errno = EINVAL; break;
    default: errno = EIO; break;
    }
    return -1;
}
#endif
#endif
