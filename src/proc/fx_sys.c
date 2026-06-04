#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <strings.h>
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

/* Framing state used for MCP input/output.
 * -1 = unknown, 0 = bare JSON, 1 = Content-Length.
 */
static int fx_mcp_framing = -1;

void fx_c_read_jsonrpc_message(char *buf, int bufsize, int *nread) {
    int content_length = -1;
    int pos = 0;
    int is_json = 0;
    int ch;
    char discard[256];
    int remaining;
    size_t n;
    size_t got;

    *nread = 0;
    if (bufsize <= 0) return;

    for (;;) {
        pos = 0;
        is_json = 0;
        for (;;) {
            ch = fgetc(stdin);
            if (ch == EOF) {
                *nread = -1;
                return;
            }
            if (ch == '\r') continue;
            if (ch == '\n') break;

            if (pos == 0 && isspace((unsigned char)ch)) continue;
            if (pos == 0) is_json = (ch == '{');

            if (is_json) {
                if (pos < bufsize) buf[pos++] = (char) ch;
            } else {
                if (pos < bufsize - 1) buf[pos++] = (char) ch;
            }
        }

        if (pos == 0) {
            if (content_length > 0) break;
            continue;
        }

        if (is_json) {
            *nread = pos < bufsize ? pos : bufsize;
            if (fx_mcp_framing < 0) fx_mcp_framing = 0;
            return;
        }

        buf[pos] = '\0';
        if (strncasecmp(buf, "content-length:", 14) == 0) {
            char *p = buf + 14;
            while (isspace((unsigned char)*p)) p++;
            if (*p == ':') {
                p++;
                while (isspace((unsigned char)*p)) p++;
                content_length = atoi(p);
            }
        }
    }

    if (content_length < 0) {
        *nread = -1;
        return;
    }

    if (content_length > bufsize) {
        if (fx_mcp_framing < 0) fx_mcp_framing = 1;
        remaining = content_length;
        while (remaining > 0) {
            n = (size_t)(remaining < (int)sizeof(discard) ? remaining : (int)sizeof(discard));
            got = fread(discard, 1, n, stdin);
            if (got == 0) break;
            remaining -= (int)got;
        }
        *nread = -2;
        return;
    }

    if (fx_mcp_framing < 0) fx_mcp_framing = 1;

    {
        int total = 0;
        while (total < content_length) {
            size_t got = fread(buf + total, 1, (size_t)(content_length - total), stdin);
            if (got == 0) break;
            total += (int)got;
        }
        *nread = total;
    }
}

int fx_c_get_mcp_framing(void) {
    return fx_mcp_framing;
}
