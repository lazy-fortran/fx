/* Native macOS name equivalence, used only for validation, never rewriting. */
#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#include <stdbool.h>
#include <stdint.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#ifdef _WIN32
#include "../../include/fx_win_store.h"
#endif
#include <string.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#include <dlfcn.h>
#include <pthread.h>
#include <sys/mount.h>
#include <sys/attr.h>
static pthread_once_t name_once = PTHREAD_ONCE_INIT;
static CFStringRef (*make_string)(CFAllocatorRef, const UInt8 *, CFIndex,
                                 CFStringEncoding, Boolean);
static CFMutableStringRef (*copy_string)(CFAllocatorRef, CFIndex, CFStringRef);
static void (*normalize_string)(CFMutableStringRef, CFStringNormalizationForm);
static CFComparisonResult (*compare_string)(CFStringRef, CFStringRef, CFStringCompareFlags);
static void (*release_string)(CFTypeRef);
static int names_available;
static void load_names(void)
{
    void *library = dlopen("/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation",
                          RTLD_LAZY | RTLD_LOCAL);
    if (!library) return;
    make_string = dlsym(library, "CFStringCreateWithBytes");
    copy_string = dlsym(library, "CFStringCreateMutableCopy");
    normalize_string = dlsym(library, "CFStringNormalize");
    compare_string = dlsym(library, "CFStringCompare");
    release_string = dlsym(library, "CFRelease");
    names_available = make_string && copy_string && normalize_string &&
                      compare_string && release_string;
}
static CFMutableStringRef normalized(const char *name)
{
    CFStringRef raw = make_string(NULL, (const UInt8 *)name, (CFIndex)strlen(name),
                                 kCFStringEncodingUTF8, false);
    CFMutableStringRef value;
    if (!raw) return NULL;
    value = copy_string(NULL, 0, raw);
    release_string(raw);
    if (value) normalize_string(value, kCFStringNormalizationFormD);
    return value;
}
#endif
int fx_immutable_name_valid(const char *name)
{
#ifdef _WIN32
    wchar_t *wide = fx_win32_wide(name);
    if (!wide) return 0;
    size_t length = wcslen(wide);
    int valid = length > 0 && wide[length - 1] != L'.' && wide[length - 1] != L' ';
    for (size_t i = 0; valid && i < length; ++i)
        if (wide[i] < 32 || wcschr(L"<>:\"/\\|?*", wide[i])) valid = 0;
    wchar_t stem[9] = {0};
    size_t used = 0;
    while (used < 8 && wide[used] && wide[used] != L'.') {
        stem[used] = wide[used]; ++used;
    }
    if (!wcscmp(stem, L"CON") || !wcscmp(stem, L"PRN") ||
        !wcscmp(stem, L"AUX") || !wcscmp(stem, L"NUL")) valid = 0;
    if (used == 4 && (stem[3] >= L'1' && stem[3] <= L'9') &&
        (!wcsncmp(stem, L"COM", 3) || !wcsncmp(stem, L"LPT", 3))) valid = 0;
    /* Device names are ASCII case insensitive. */
    for (size_t i = 0; i < used; ++i)
        if (stem[i] >= L'a' && stem[i] <= L'z') stem[i] -= L'a' - L'A';
    if (!wcscmp(stem, L"CON") || !wcscmp(stem, L"PRN") ||
        !wcscmp(stem, L"AUX") || !wcscmp(stem, L"NUL")) valid = 0;
    if (used == 4 && (stem[3] >= L'1' && stem[3] <= L'9') &&
        (!wcsncmp(stem, L"COM", 3) || !wcsncmp(stem, L"LPT", 3))) valid = 0;
    free(wide); return valid;
#elif defined(__APPLE__)
    pthread_once(&name_once, load_names);
    if (!names_available) return 0;
    CFMutableStringRef value = normalized(name);
    if (!value) return 0;
    release_string(value);
#else
    (void)name;
#endif
    return 1;
}
/* Comparison uses exactly the native equivalence relation above. Sorting an
 * index avoids comparing every pair and leaves manifest byte order untouched. */
typedef struct {
#ifdef _WIN32
    wchar_t *value;
#elif defined(__APPLE__)
    CFMutableStringRef value;
#else
    const char *value;
#endif
} fx_name_key;

static int name_key_init(fx_name_key *key, const char *name)
{
#ifdef _WIN32
    key->value = fx_win32_wide(name);
#elif defined(__APPLE__)
    pthread_once(&name_once, load_names);
    if (!names_available) return 0;
    key->value = normalized(name);
#else
    key->value = name;
#endif
    return key->value != NULL;
}

static void name_key_destroy(fx_name_key *key)
{
#ifdef _WIN32
    free(key->value);
#elif defined(__APPLE__)
    if (key->value) release_string(key->value);
#else
    (void)key;
#endif
}

static int name_key_compare(const fx_name_key *left, const fx_name_key *right,
                            int *valid)
{
#ifdef _WIN32
    int comparison = CompareStringOrdinal(left->value, -1, right->value, -1, TRUE);
    if (!comparison) { *valid = 0; return 0; }
    return comparison - CSTR_EQUAL;
#elif defined(__APPLE__)
    (void)valid;
    return (int)compare_string(left->value, right->value, kCFCompareCaseInsensitive);
#else
    (void)valid;
    const unsigned char *a = (const unsigned char *)left->value;
    const unsigned char *b = (const unsigned char *)right->value;
    for (size_t i = 0; ; ++i) {
        unsigned char x = a[i], y = b[i];
        if (x >= 'A' && x <= 'Z') x += 'a' - 'A';
        if (y >= 'A' && y <= 'Z') y += 'a' - 'A';
        if (x != y) return x < y ? -1 : 1;
        if (!x) return 0;
    }
#endif
}

int fx_immutable_names_equivalent(const char *a, const char *b)
{
    fx_name_key left = {0}, right = {0};
    int valid = name_key_init(&left, a) && name_key_init(&right, b);
    int comparison = valid ? name_key_compare(&left, &right, &valid) : 0;
    name_key_destroy(&left);
    name_key_destroy(&right);
    return valid ? comparison == 0 : -1;
}

int fx_immutable_names_unique(const char *names, const size_t *offsets, size_t count)
{
    if (count < 2) return 1;
    if (count > SIZE_MAX / sizeof(fx_name_key) ||
        count > SIZE_MAX / sizeof(size_t)) return -1;
    fx_name_key *keys = calloc(count, sizeof(*keys));
    size_t *order = malloc(count * sizeof(*order));
    size_t *scratch = malloc(count * sizeof(*scratch));
    size_t initialized = 0;
    int result = -1, valid = 1;
    if (!keys || !order || !scratch) goto done;
    for (size_t i = 0; i < count; ++i) {
        if (!name_key_init(&keys[i], names + offsets[i])) goto done;
        initialized++;
        order[i] = i;
    }
    for (size_t width = 1; width < count; ) {
        for (size_t begin = 0; begin < count; begin += 2 * width) {
            size_t middle = begin + (count - begin < width ? count - begin : width);
            size_t end = middle + (count - middle < width ? count - middle : width);
            size_t a = begin, b = middle, out = begin;
            while (a < middle && b < end) {
                int comparison = name_key_compare(&keys[order[a]], &keys[order[b]], &valid);
                if (!valid) goto done;
                if (!comparison) { result = 0; goto done; }
                scratch[out++] = comparison < 0 ? order[a++] : order[b++];
            }
            while (a < middle) scratch[out++] = order[a++];
            while (b < end) scratch[out++] = order[b++];
        }
        size_t *swap = order; order = scratch; scratch = swap;
        if (width > count / 2) break;
        width *= 2;
    }
    result = 1;
done:
    for (size_t i = 0; i < initialized; ++i) name_key_destroy(&keys[i]);
    free(keys); free(order); free(scratch);
    return result;
}
/* Independent OS oracle: actual exclusive creation reports volume aliasing. */
int fx_immutable_probe_alias(const char *directory, const char *a, const char *b)
{
    int dir = open(directory, O_RDONLY | O_DIRECTORY | O_NOFOLLOW);
    int first, second, saved;
    if (dir < 0) return -1;
    first = openat(dir, a, O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW, 0600);
    if (first < 0) { close(dir); return -1; }
    close(first);
    second = openat(dir, b, O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW, 0600);
    saved = errno;
    if (second >= 0) { close(second); unlinkat(dir, b, 0); }
    unlinkat(dir, a, 0);
    close(dir);
    if (second < 0 && saved != EEXIST) return -1;
    return second < 0 ? 1 : 0;
}
int fx_immutable_is_apfs(const char *path)
{
#ifdef __APPLE__
    struct statfs info;
    if (statfs(path, &info) != 0) return -1;
    return strcmp(info.f_fstypename, "apfs") == 0;
#else
    (void)path;
    return 0;
#endif
}
int fx_immutable_clone_id(const char *path, long long *id)
{
#ifdef __APPLE__
    struct attrlist wanted;
    struct { uint32_t length; attribute_set_t returned; uint64_t id; } value;
    memset(&wanted, 0, sizeof(wanted));
    memset(&value, 0, sizeof(value));
    wanted.bitmapcount = ATTR_BIT_MAP_COUNT;
    wanted.commonattr = ATTR_CMN_RETURNED_ATTRS;
    wanted.forkattr = ATTR_CMNEXT_CLONEID;
    if (getattrlist(path, &wanted, &value, sizeof(value), FSOPT_ATTR_CMN_EXTENDED) != 0)
        return -1;
    if (!(value.returned.forkattr & ATTR_CMNEXT_CLONEID)) return -1;
    *id = (long long)value.id;
    return 0;
#else
    (void)path; (void)id;
    return -1;
#endif
}
