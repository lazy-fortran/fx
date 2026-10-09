#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <bcrypt.h>
#elif defined(__APPLE__)
#include <dlfcn.h>
#include <pthread.h>
#else
#include <dlfcn.h>
#include <pthread.h>
#endif

static void hex_encode(const unsigned char in[32], char out[64]) {
    static const char hex[] = "0123456789abcdef";
    for (int i = 0; i < 32; ++i) {
        out[2 * i] = hex[(in[i] >> 4) & 15];
        out[2 * i + 1] = hex[in[i] & 15];
    }
}

#if defined(_WIN32)
struct bcrypt_api {
    HMODULE library;
    NTSTATUS (WINAPI *open)(BCRYPT_ALG_HANDLE *, LPCWSTR, LPCWSTR, ULONG);
    NTSTATUS (WINAPI *close)(BCRYPT_ALG_HANDLE, ULONG);
    NTSTATUS (WINAPI *property)(BCRYPT_HANDLE, LPCWSTR, PUCHAR, ULONG, ULONG *, ULONG);
    NTSTATUS (WINAPI *create)(BCRYPT_ALG_HANDLE, BCRYPT_HASH_HANDLE *, PUCHAR,
                             ULONG, PUCHAR, ULONG, ULONG);
    NTSTATUS (WINAPI *update)(BCRYPT_HASH_HANDLE, PUCHAR, ULONG, ULONG);
    NTSTATUS (WINAPI *finish)(BCRYPT_HASH_HANDLE, PUCHAR, ULONG, ULONG);
    NTSTATUS (WINAPI *destroy)(BCRYPT_HASH_HANDLE);
};
static struct bcrypt_api bcrypt;
static INIT_ONCE bcrypt_once = INIT_ONCE_STATIC_INIT;

static BOOL CALLBACK load_bcrypt(PINIT_ONCE once, PVOID parameter, PVOID *context) {
    (void)once; (void)parameter; (void)context;
    HMODULE library = LoadLibraryExW(L"bcrypt.dll", NULL, LOAD_LIBRARY_SEARCH_SYSTEM32);
    if (!library) return TRUE;
#define LOAD(field, symbol) do { \
    FARPROC function = GetProcAddress(library, symbol); \
    if (!function) { FreeLibrary(library); return TRUE; } \
    memcpy(&bcrypt.field, &function, sizeof(function)); \
} while (0)
    LOAD(open, "BCryptOpenAlgorithmProvider");
    LOAD(close, "BCryptCloseAlgorithmProvider");
    LOAD(property, "BCryptGetProperty");
    LOAD(create, "BCryptCreateHash");
    LOAD(update, "BCryptHashData");
    LOAD(finish, "BCryptFinishHash");
    LOAD(destroy, "BCryptDestroyHash");
#undef LOAD
    bcrypt.library = library;
    return TRUE;
}

static int bcrypt_available(void) {
    return InitOnceExecuteOnce(&bcrypt_once, load_bcrypt, NULL, NULL) && bcrypt.library;
}
int fx_c_sha256_available(void) {
    if (!bcrypt_available()) return 0;
    BCRYPT_ALG_HANDLE alg = NULL;
    NTSTATUS st = bcrypt.open(&alg, BCRYPT_SHA256_ALGORITHM, NULL, 0);
    if (st == 0 && alg) bcrypt.close(alg, 0);
    return st == 0 ? 1 : 0;
}

int fx_c_sha256_digest(const unsigned char *data, int n, char *out_hex) {
    if (!bcrypt_available()) return 1;
    BCRYPT_ALG_HANDLE alg = NULL;
    BCRYPT_HASH_HANDLE hash = NULL;
    unsigned char digest[32];
    DWORD obj_len = 0, got = 0;
    unsigned char *obj = NULL;
    NTSTATUS st;

    st = bcrypt.open(&alg, BCRYPT_SHA256_ALGORITHM, NULL, 0);
    if (st != 0) return 1;
    st = bcrypt.property(alg, BCRYPT_OBJECT_LENGTH, (PUCHAR)&obj_len,
                           sizeof(obj_len), &got, 0);
    if (st != 0) goto fail;
    obj = (unsigned char *)HeapAlloc(GetProcessHeap(), 0, obj_len);
    if (!obj) goto fail;
    st = bcrypt.create(alg, &hash, obj, obj_len, NULL, 0, 0);
    if (st != 0) goto fail;
    st = bcrypt.update(hash, (PUCHAR)data, (ULONG)n, 0);
    if (st != 0) goto fail;
    st = bcrypt.finish(hash, digest, sizeof(digest), 0);
    if (st != 0) goto fail;
    hex_encode(digest, out_hex);
    bcrypt.destroy(hash);
    HeapFree(GetProcessHeap(), 0, obj);
    bcrypt.close(alg, 0);
    return 0;
fail:
    if (hash) bcrypt.destroy(hash);
    if (obj) HeapFree(GetProcessHeap(), 0, obj);
    if (alg) bcrypt.close(alg, 0);
    return 1;
}
#elif defined(__APPLE__)
typedef unsigned char *(*cc_sha256_fn)(const void *, unsigned int, unsigned char *);
static pthread_once_t cc_once = PTHREAD_ONCE_INIT;
static cc_sha256_fn cc_digest = NULL;

static void load_cc_sha256_once(void) {
    void *handle = dlopen("/usr/lib/system/libcommonCrypto.dylib", RTLD_LAZY);
    if (!handle) handle = dlopen("/usr/lib/libSystem.dylib", RTLD_LAZY);
    if (handle) cc_digest = (cc_sha256_fn)dlsym(handle, "CC_SHA256");
}

static cc_sha256_fn load_cc_sha256(void) {
    if (pthread_once(&cc_once, load_cc_sha256_once) != 0) return NULL;
    return cc_digest;
}

int fx_c_sha256_available(void) {
    return load_cc_sha256() ? 1 : 0;
}

int fx_c_sha256_digest(const unsigned char *data, int n, char *out_hex) {
    unsigned char digest[32];
    cc_sha256_fn fn = load_cc_sha256();
    if (!fn) return 1;
    if (!fn(data, (unsigned int)n, digest)) return 1;
    hex_encode(digest, out_hex);
    return 0;
}
#else
typedef unsigned char *(*openssl_sha256_fn)(const unsigned char *, size_t,
                                            unsigned char *);
static pthread_once_t openssl_once = PTHREAD_ONCE_INIT;
static openssl_sha256_fn openssl_digest = NULL;

static void load_openssl_sha256_once(void) {
    void *handle = NULL;
    const char *libs[] = {"libcrypto.so.3", "libcrypto.so.1.1", "libcrypto.so", NULL};
    for (int i = 0; libs[i]; ++i) {
        handle = dlopen(libs[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle) break;
    }
    if (handle) openssl_digest = (openssl_sha256_fn)dlsym(handle, "SHA256");
}

static openssl_sha256_fn load_openssl_sha256(void) {
    if (pthread_once(&openssl_once, load_openssl_sha256_once) != 0) return NULL;
    return openssl_digest;
}

int fx_c_sha256_available(void) {
    return load_openssl_sha256() ? 1 : 0;
}

int fx_c_sha256_digest(const unsigned char *data, int n, char *out_hex) {
    unsigned char digest[32];
    openssl_sha256_fn fn = load_openssl_sha256();
    if (!fn) return 1;
    if (!fn(data, (size_t)n, digest)) return 1;
    hex_encode(digest, out_hex);
    return 0;
}
#endif
