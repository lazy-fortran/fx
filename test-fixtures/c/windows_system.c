/* Independent native Win32 oracle for Fx process and UTF-8 file APIs. */
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <io.h>
#include <fcntl.h>

int fx_c_exec(const char *, int, char *, int *, char *, int *);
int fx_c_exec_silent(const char *, int);
int fx_c_file_read(const char *, char *, int *);
int fx_c_file_write(const char *, const char *, int);
int fx_c_mkdir_p(const char *);
int fx_c_rename(const char *, const char *);
int fx_c_unlink(const char *);
int fx_c_rmdir(const char *);
int fx_c_file_fingerprint(const char *, long long *, long long *);
int fx_c_set_mtime(const char *, long long);
int fx_c_scan_dir(const char *, const char *, int, char *, int *, int);
int fx_c_count_dirs(const char *, int *);
int fx_c_count_files(const char *, int *);
int fx_c_collect_files(const char *, char *, int, int *, int);
wchar_t *fx_win32_command_line(const char *, int, int);

static int failures;
static void check(int success, const char *name)
{ printf("%s: %s\n", success ? "PASS" : "FAIL", name); if (!success) ++failures; }

static void append(char *packed, size_t capacity, int *length, const char *text)
{
    size_t n = strlen(text) + 1;
    if (n > capacity - (size_t)*length) abort();
    memcpy(packed + *length, text, n); *length += (int)n;
}

int wmain(int argc, wchar_t **argv)
{
    wchar_t executable[32768];
    char image[65536], packed[65536], output[128], error[128], files[4 * 512];
    char root[512], file[1024], moved[1024], nested[600], scan[2048], deep[512];
    int length, out_length, err_length, count, rc, i;
    long long size, before, after;
    if (argc > 1 && !wcscmp(argv[1], L"--dual")) {
        char bytes[1024];
        _setmode(_fileno(stdout), _O_BINARY); _setmode(_fileno(stderr), _O_BINARY);
        for (i = 0; i < 96; ++i) {
            memset(bytes, 'O', sizeof(bytes)); fwrite(bytes, 1, sizeof(bytes), stdout);
            memset(bytes, 'E', sizeof(bytes)); fwrite(bytes, 1, sizeof(bytes), stderr);
        }
        return 37;
    }
    if (argc > 1 && !wcscmp(argv[1], L"--arguments")) {
        if (argc != 8 || wcscmp(argv[2], L"") || wcscmp(argv[3], L"space tab\t") ||
            wcscmp(argv[4], L"quote\"inside") || wcscmp(argv[5], L"tail\\") ||
            wcscmp(argv[6], L"slash\\\"quote") ||
            wcscmp(argv[7], L"\x03bb\x4e2d\xd83d\xde80")) return 19;
        return 0;
    }
    if (!GetModuleFileNameW(NULL, executable, 32768)) return 2;
    if (!WideCharToMultiByte(CP_UTF8, 0, executable, -1, image, sizeof(image), NULL, NULL))
        return 2;
    length = 0; append(packed, sizeof(packed), &length, image);
    append(packed, sizeof(packed), &length, "--arguments");
    append(packed, sizeof(packed), &length, "");
    append(packed, sizeof(packed), &length, "space tab\t");
    append(packed, sizeof(packed), &length, "quote\"inside");
    append(packed, sizeof(packed), &length, "tail\\");
    append(packed, sizeof(packed), &length, "slash\\\"quote");
    append(packed, sizeof(packed), &length, "\xce\xbb\xe4\xb8\xad\xf0\x9f\x9a\x80");
    check(fx_c_exec_silent(packed, 8) == 0,
           "real native CRT argv preserves empty/space/tab/quote/backslash/Unicode");
    check(!fx_win32_command_line("broken", 6, 1), "unterminated packed input rejects");
    check(!fx_win32_command_line("a\0extra\0", 8, 1), "extra packed arguments reject");
    length = 0; append(packed, sizeof(packed), &length, image);
    append(packed, sizeof(packed), &length, "--dual");
    out_length = 17; err_length = 19;
    rc = fx_c_exec(packed, 2, output, &out_length, error, &err_length);
    check(rc == 37 && out_length == 17 && err_length == 19,
           "both overfull pipe streams drain without deadlock and retain exit37");
    check(!memcmp(output, "OOOOOOOOOOOOOOOOO", 17) &&
           !memcmp(error, "EEEEEEEEEEEEEEEEEEE", 19), "captured bytes stay exact");
    snprintf(root, sizeof(root), "fx-system-%lu-\xce\xbb\xe4\xb8\xad\xf0\x9f\x9a\x80",
              (unsigned long)GetCurrentProcessId());
    snprintf(nested, sizeof(nested), "%s/nested", root);
    snprintf(file, sizeof(file), "%s/probe.f90", nested);
    snprintf(moved, sizeof(moved), "%s/renamed.f90", root);
    check(fx_c_mkdir_p(nested) == 0, "recursive Unicode directories materialize");
    check(fx_c_file_write(file, "first\0bytes", 11) == 0, "Unicode file writes binary bytes");
    length = sizeof(output); memset(output, 0xff, sizeof(output));
    check(fx_c_file_read(file, output, &length) == 0 && length == 11 &&
           !memcmp(output, "first\0bytes", 11), "binary file read preserves embedded NUL");
    check(fx_c_file_fingerprint(file, &size, &before) == 0 && size == 11,
           "fingerprint reports actual byte count");
    check(fx_c_set_mtime(file, 1700000000) == 0 &&
           fx_c_file_fingerprint(file, &size, &after) == 0 && after == 1700000000000000000LL,
           "native timestamp has exact Unix nanoseconds");
    check(fx_c_count_dirs(root, &count) == 0 && count == 2, "directory inventory includes root");
    check(fx_c_count_files(root, &count) == 0 && count == 1, "file inventory is exact");
    check(fx_c_collect_files(root, files, 512, &count, 4) == 0 && count == 1 &&
           !strcmp(files, file), "collected UTF8 filename names the real file");
    check(fx_c_scan_dir(root, ".f90\0", 1, scan, &count, 2) == 0 && count == 1 &&
           !strcmp(scan, file), "extension scan preserves supported source membership");
    check(fx_c_rename(file, moved) == 0, "rename preserves Unicode namespace");
    check(fx_c_file_write(file, "second", 6) == 0 && fx_c_rename(file, moved) == 0,
           "existing destination replacement is supported");
    length = sizeof(output);
    check(fx_c_file_read(moved, output, &length) == 0 && length == 6 &&
           !memcmp(output, "second", 6), "replacement publishes complete new bytes");
    check(fx_c_unlink(moved) == 0 && fx_c_rmdir(nested) == 0 && fx_c_rmdir(root) == 0,
           "owned generated material is removed");
    snprintf(deep, sizeof(deep), "%s", root);
    for (i = 0; i < 8; ++i) {
        size_t used = strlen(deep);
        memset(deep + used + 1, 'a' + i, 40);
        deep[used] = '/'; deep[used + 41] = 0;
    }
    snprintf(file, sizeof(file), "%s/long.f90", deep);
    check(strlen(file) > MAX_PATH && fx_c_mkdir_p(deep) == 0 &&
           fx_c_file_write(file, "long-path", 9) == 0,
           "long native filename exceeds MAX_PATH without truncation");
    length = sizeof(output);
    check(fx_c_file_read(file, output, &length) == 0 && length == 9 &&
           !memcmp(output, "long-path", 9), "long filename returns complete bytes");
    check(fx_c_unlink(file) == 0, "long file cleanup succeeds");
    for (i = 0; i < 8; ++i) {
        char *slash;
        check(fx_c_rmdir(deep) == 0, "long generated directory cleanup succeeds");
        slash = strrchr(deep, '/'); if (!slash) abort(); *slash = 0;
    }
    check(fx_c_rmdir(root) == 0, "long fixture root cleanup succeeds");
    printf("windows-system: %d failures\n", failures);
    return failures ? 1 : 0;
}
