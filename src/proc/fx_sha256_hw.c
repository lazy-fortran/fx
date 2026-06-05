#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <bcrypt.h>
#elif defined(__APPLE__)
#include <dlfcn.h>
#else
#include <dlfcn.h>
#endif

static void hex_encode(const unsigned char in[32], char out[64]) {
    static const char hex[] = "0123456789abcdef";
    for (int i = 0; i < 32; ++i) {
        out[2 * i] = hex[(in[i] >> 4) & 15];
        out[2 * i + 1] = hex[in[i] & 15];
    }
}

#if defined(_WIN32)
int fx_c_sha256_available(void) {
    BCRYPT_ALG_HANDLE alg = NULL;
    NTSTATUS st = BCryptOpenAlgorithmProvider(&alg, BCRYPT_SHA256_ALGORITHM, NULL, 0);
    if (st == 0 && alg) BCryptCloseAlgorithmProvider(alg, 0);
    return st == 0 ? 1 : 0;
}

int fx_c_sha256_digest(const unsigned char *data, int n, char *out_hex) {
    BCRYPT_ALG_HANDLE alg = NULL;
    BCRYPT_HASH_HANDLE hash = NULL;
    unsigned char digest[32];
    DWORD obj_len = 0, got = 0;
    unsigned char *obj = NULL;
    NTSTATUS st;

    st = BCryptOpenAlgorithmProvider(&alg, BCRYPT_SHA256_ALGORITHM, NULL, 0);
    if (st != 0) return 1;
    st = BCryptGetProperty(alg, BCRYPT_OBJECT_LENGTH, (PUCHAR)&obj_len,
                           sizeof(obj_len), &got, 0);
    if (st != 0) goto fail;
    obj = (unsigned char *)HeapAlloc(GetProcessHeap(), 0, obj_len);
    if (!obj) goto fail;
    st = BCryptCreateHash(alg, &hash, obj, obj_len, NULL, 0, 0);
    if (st != 0) goto fail;
    st = BCryptHashData(hash, (PUCHAR)data, (ULONG)n, 0);
    if (st != 0) goto fail;
    st = BCryptFinishHash(hash, digest, sizeof(digest), 0);
    if (st != 0) goto fail;
    hex_encode(digest, out_hex);
    BCryptDestroyHash(hash);
    HeapFree(GetProcessHeap(), 0, obj);
    BCryptCloseAlgorithmProvider(alg, 0);
    return 0;
fail:
    if (hash) BCryptDestroyHash(hash);
    if (obj) HeapFree(GetProcessHeap(), 0, obj);
    if (alg) BCryptCloseAlgorithmProvider(alg, 0);
    return 1;
}
#elif defined(__APPLE__)
typedef unsigned char *(*cc_sha256_fn)(const void *, unsigned int, unsigned char *);

static cc_sha256_fn load_cc_sha256(void) {
    static void *handle = NULL;
    static cc_sha256_fn fn = NULL;
    if (fn) return fn;
    handle = dlopen("/usr/lib/system/libcommonCrypto.dylib", RTLD_LAZY);
    if (!handle) handle = dlopen("/usr/lib/libSystem.dylib", RTLD_LAZY);
    if (!handle) return NULL;
    fn = (cc_sha256_fn)dlsym(handle, "CC_SHA256");
    return fn;
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

static openssl_sha256_fn load_openssl_sha256(void) {
    static void *handle = NULL;
    static openssl_sha256_fn fn = NULL;
    const char *libs[] = {"libcrypto.so.3", "libcrypto.so.1.1", "libcrypto.so", NULL};
    if (fn) return fn;
    for (int i = 0; libs[i]; ++i) {
        handle = dlopen(libs[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle) break;
    }
    if (!handle) return NULL;
    fn = (openssl_sha256_fn)dlsym(handle, "SHA256");
    return fn;
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
