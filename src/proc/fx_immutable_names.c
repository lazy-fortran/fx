/* Native macOS name equivalence, used only for validation, never rewriting. */
#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#include <stdbool.h>
#include <stdint.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
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
#ifdef __APPLE__
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
int fx_immutable_names_equivalent(const char *a, const char *b)
{
#ifdef __APPLE__
    pthread_once(&name_once, load_names);
    if (!names_available) return -1;
    CFMutableStringRef left = normalized(a), right = normalized(b);
    if (!left || !right) {
        if (left) release_string(left);
        if (right) release_string(right);
        return -1;
    }
    int equal = compare_string(left, right, kCFCompareCaseInsensitive) == kCFCompareEqualTo;
    release_string(left);
    release_string(right);
    return equal;
#else
    /* ASCII case collisions cannot be portably materialized on default APFS. */
    if (strlen(a) != strlen(b)) return 0;
    for (size_t i = 0; a[i]; ++i) {
        unsigned char left = (unsigned char)a[i], right = (unsigned char)b[i];
        if (left >= 'A' && left <= 'Z') left += 'a' - 'A';
        if (right >= 'A' && right <= 'Z') right += 'a' - 'A';
        if (left != right) return 0;
    }
    return 1;
#endif
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
