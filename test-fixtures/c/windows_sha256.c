#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <string.h>
extern int fx_c_sha256_available(void);
extern int fx_c_sha256_digest(const unsigned char *, int, char *);
static HANDLE start;
static const char foo[] = "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae";
static DWORD WINAPI worker(void *argument) {
    (void)argument;
    if (WaitForSingleObject(start, INFINITE) != WAIT_OBJECT_0) return 1;
    for (int index = 0; index < 32; ++index) {
        char digest[64];
        if (!fx_c_sha256_available() || fx_c_sha256_digest((const unsigned char *)"foo", 3, digest) ||
            memcmp(digest, foo, 64)) return 1;
    }
    return 0;
}
int main(void) {
    HANDLE threads[8] = {0}; int failures = 0;
    start = CreateEventW(NULL, TRUE, FALSE, NULL); if (!start) return 2;
    for (int index = 0; index < 8; ++index) {
        threads[index] = CreateThread(NULL, 0, worker, NULL, 0, NULL);
        if (!threads[index]) ++failures;
    }
    SetEvent(start);
    for (int index = 0; index < 8; ++index) if (threads[index]) {
        DWORD result = 1;
        if (WaitForSingleObject(threads[index], 10000) != WAIT_OBJECT_0 ||
            !GetExitCodeThread(threads[index], &result) || result) ++failures;
        CloseHandle(threads[index]);
    }
    CloseHandle(start);
    const char *input[] = {"", "abc"};
    const char *expected[] = {
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"};
    for (int index = 0; index < 2; ++index) {
        char digest[64];
        if (fx_c_sha256_digest((const unsigned char *)input[index], (int)strlen(input[index]), digest) ||
            memcmp(digest, expected[index], 64)) ++failures;
    }
    printf("native SHA256 known vectors and concurrent first load: failures=%d\n", failures);
    return failures ? 1 : 0;
}
