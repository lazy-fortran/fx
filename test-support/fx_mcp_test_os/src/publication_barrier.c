/* Test-only syscall interposition: stop at the real post-validation fsync.
 * Production has no fixture hooks and does not acquire directory flocks. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#elif defined(__APPLE__)
#include <dlfcn.h>
#endif

static char held_path[4096], ready_path[4096], release_path[4096];

int fx_test_hold_fsync(const char *held, const char *ready, const char *release)
{
    if (strlen(held) >= sizeof held_path || strlen(ready) >= sizeof ready_path ||
        strlen(release) >= sizeof release_path) return -1;
    strcpy(held_path, held);
    strcpy(ready_path, ready);
    strcpy(release_path, release);
    return 0;
}

#if defined(__linux__) || defined(__APPLE__)
int fsync(int fd)
{
    struct stat actual, expected;
    struct timespec begin, now, delay = {0, 10000000};
    int rc, marker;
#ifdef __linux__
    rc = (int)syscall(SYS_fsync, fd);
#else
    int (*real_fsync)(int) = (int (*)(int))dlsym(RTLD_NEXT, "fsync");
    if (!real_fsync) { errno = ENOSYS; return -1; }
    rc = real_fsync(fd);
#endif
    if (rc || !held_path[0] || fstat(fd, &actual) ||
        (actual.st_mode & 0777) != 0444 || stat(held_path, &expected) || actual.st_dev != expected.st_dev ||
        actual.st_ino != expected.st_ino) return rc;
    held_path[0] = '\0';
    marker = open(ready_path, O_CREAT | O_WRONLY | O_EXCL, 0600);
    if (marker < 0) return -1;
    if (close(marker)) return -1;
    if (clock_gettime(CLOCK_MONOTONIC, &begin)) return -1;
    while (access(release_path, F_OK)) {
        if (errno != ENOENT) return -1;
        if (clock_gettime(CLOCK_MONOTONIC, &now)) return -1;
        if (now.tv_sec - begin.tv_sec >= 30) { errno = ETIMEDOUT; return -1; }
        (void)nanosleep(&delay, NULL);
    }
    return rc;
}
#endif
