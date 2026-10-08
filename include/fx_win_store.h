/* Native Win32 operations used by the existing immutable-store engine. */
#ifndef FX_WIN_STORE_H
#define FX_WIN_STORE_H
#ifdef _WIN32
#include "fx_win32.h"
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <stdio.h>

/* These flags have real native semantics; directory/reparse checks are mandatory. */
#define O_DIRECTORY 0x01000000
#define O_NOFOLLOW  0x02000000
#define O_CLOEXEC   0x04000000
#define AT_FDCWD (-100)
#define AT_SYMLINK_NOFOLLOW 0x100
#define AT_SYMLINK_FOLLOW 0x400
#define AT_REMOVEDIR 0x200
#define UTIME_OMIT 1073741822L
#define UTIME_NOW 1073741823L
#ifndef LOCK_SH
#define LOCK_SH 1
#define LOCK_EX 2
#define LOCK_NB 4
#define LOCK_UN 8
#endif
#ifndef S_IFLNK
#define S_IFLNK 0120000
#define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
#endif
#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif

struct fx_win_stat {
    uint64_t st_dev, st_ino;
    uint64_t st_nlink;
    int64_t st_size, st_blocks;
    time_t st_mtime, st_ctime;
    struct timespec st_mtim, st_ctim;
    unsigned st_mode;
};
struct fx_win_dirent { char d_name[32768]; };
typedef struct fx_win_directory fx_win_directory;
int fx_win_open(const char *, int, ...);
int fx_win_openat(int, const char *, int, ...);
int fx_win_close(int);
int fx_win_fstat(int, struct fx_win_stat *);
int fx_win_fstatat(int, const char *, struct fx_win_stat *, int);
int fx_win_lstat(const char *, struct fx_win_stat *);
int fx_win_stat(const char *, struct fx_win_stat *);
int fx_win_mkdir(const char *, int);
int fx_win_mkdirat(int, const char *, int);
int fx_win_unlinkat(int, const char *, int);
int fx_win_unlink(const char *);
int fx_win_rmdir(const char *);
int fx_win_renameat(int, const char *, int, const char *);
int fx_win_rename_exclusive(int, const char *, int, const char *);
int fx_win_link_fd(int, int, const char *);
int fx_win_fsync(int);
int fx_win_fchmod(int, mode_t);
int fx_win_chmod(const char *, mode_t);
int fx_win_flock(int, int);
int fx_win_private_owned(int);
ssize_t fx_win_readlinkat(int, const char *, char *, size_t);
int fx_win_symlink(const char *, const char *);
int fx_win_faccessat(int, const char *, int, int);
ssize_t fx_win_pread(int, void *, size_t, int64_t);
ssize_t fx_win_pwrite(int, const void *, size_t, int64_t);
int fx_win_ftruncate(int, int64_t);
int fx_win_futimens(int, const struct timespec [2]);
ssize_t fx_win_getline(char **, size_t *, FILE *);
fx_win_directory *fx_win_opendir(const char *);
fx_win_directory *fx_win_fdopendir(int);
struct fx_win_dirent *fx_win_readdir(fx_win_directory *);
int fx_win_dirfd(fx_win_directory *);
int fx_win_closedir(fx_win_directory *);
void fx_win_rewinddir(fx_win_directory *);
void fx_win_seekdir(fx_win_directory *, long);
long fx_win_telldir(fx_win_directory *);
int fx_win_open_directory(const char *);
int fx_win_mkdirs(const char *, int);
int fx_win_mkdirs_mode(const char *, int, int);
int fx_win_resolve_root(const char *, char *, size_t);
char *fx_win_getcwd(char *, size_t);
char *fx_win_realpath(const char *, char *);
char *fx_win_mkdtemp(char *);
int fx_win_mkstemp(char *);
int fx_win_rename(const char *, const char *);

#ifndef FX_WIN_STORE_IMPLEMENTATION
#define stat fx_win_stat
#define dev_t uint64_t
#define ino_t uint64_t
#define open fx_win_open
#define openat fx_win_openat
#define close fx_win_close
#define fstat fx_win_fstat
#define fstatat fx_win_fstatat
#define lstat fx_win_lstat
#define mkdir fx_win_mkdir
#define mkdirat fx_win_mkdirat
#define unlinkat fx_win_unlinkat
#define unlink fx_win_unlink
#define rmdir fx_win_rmdir
#define renameat fx_win_renameat
#define rename fx_win_rename
#define fsync fx_win_fsync
#define fchmod fx_win_fchmod
#define chmod fx_win_chmod
#define flock fx_win_flock
#define readlinkat fx_win_readlinkat
#define symlink fx_win_symlink
#define faccessat fx_win_faccessat
#define pread fx_win_pread
#define pwrite fx_win_pwrite
#define ftruncate fx_win_ftruncate
#define futimens fx_win_futimens
#define getline fx_win_getline
#define DIR fx_win_directory
#define dirent fx_win_dirent
#define opendir fx_win_opendir
#define fdopendir fx_win_fdopendir
#define readdir fx_win_readdir
#define dirfd fx_win_dirfd
#define closedir fx_win_closedir
#define rewinddir fx_win_rewinddir
#define seekdir fx_win_seekdir
#define telldir fx_win_telldir
#define getcwd fx_win_getcwd
#define realpath fx_win_realpath
#define mkdtemp fx_win_mkdtemp
#define mkstemp fx_win_mkstemp
#endif
#endif
#endif
