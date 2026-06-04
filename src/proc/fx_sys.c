#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <dirent.h>
#include <signal.h>
#include <errno.h>

/*
 * fx_sys.c: C implementations for fx_proc.f90 Fortran interfaces.
 * Provides process execution, directory scanning, file I/O, and
 * signal handling via POSIX APIs.
 */

/* Fork/execvp with stdout+stderr capture via pipes. */
int fx_c_exec(const char *argv, int n_argv,
              char *stdout_buf, int *stdout_len,
              char *stderr_buf, int *stderr_len)
{
    (void)argv;
    (void)n_argv;
    *stdout_len = 0;
    *stderr_len = 0;
    stdout_buf[0] = '\0';
    stderr_buf[0] = '\0';
    return -1; /* not implemented */
}

/* Fork/execvp without output capture. Returns exit code. */
int fx_c_exec_silent(const char *argv, int n_argv)
{
    (void)argv;
    (void)n_argv;
    return -1; /* not implemented */
}

/*
 * Recursive directory scan.
 * Walks root, collects files matching given extensions.
 * Skips .git, build, node_modules directories.
 * Returns sorted file list.
 */
/* !$omp parallel: future parallelism for large directory trees */
int fx_c_scan_dir(const char *root, const char *extensions, int n_ext,
                  char *files, int *n_files, int max_files)
{
    (void)root;
    (void)extensions;
    (void)n_ext;
    (void)files;
    (void)max_files;
    *n_files = 0;
    return -1; /* not implemented */
}

/* Read entire file into buffer. Returns 0 on success, -1 on error. */
int fx_c_file_read(const char *path, char *content, int *n_bytes)
{
    (void)path;
    (void)content;
    *n_bytes = 0;
    return -1; /* not implemented */
}

/* Write buffer to file. Returns 0 on success, -1 on error. */
int fx_c_file_write(const char *path, const char *content, int n_bytes)
{
    (void)path;
    (void)content;
    (void)n_bytes;
    return -1; /* not implemented */
}

/* Create a temp file with given prefix. Returns path and length. */
void fx_c_tmpfile(const char *prefix, char *path, int *path_len)
{
    (void)prefix;
    path[0] = '\0';
    *path_len = 0;
}

/* Return current process ID. */
int fx_c_pid(void)
{
    return (int)getpid();
}

/* Send signal to process. Returns 0 on success, -1 on error. */
int fx_c_kill(int pid, int signal)
{
    return kill((pid_t)pid, signal);
}
