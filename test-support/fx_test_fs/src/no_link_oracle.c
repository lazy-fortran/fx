/* Test-only unsupported-cross-directory-link filesystem adapter.
 * Compile the exact implementation with isolated symbols; same-directory
 * links remain real syscalls, so the copy fallback must publish actual bytes.
 */
#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#ifdef __APPLE__
#include <sys/clonefile.h>
#endif
static int attempts;
static int no_cross_link(int from, const char *source, int to,
                         const char *destination, int flags)
{
    if (from != to) { ++attempts; errno = EXDEV; return -1; }
    return linkat(from, source, to, destination, flags);
}
#ifdef __APPLE__
static int no_clone(int fd, int directory, const char *name, unsigned flags)
{
    (void)fd; (void)directory; (void)name; (void)flags;
    ++attempts; errno = EXDEV; return -1;
}
#define fclonefileat no_clone
#endif
#define linkat no_cross_link
#define fx_owned_begin_at fx_test_nolink_owned_begin_at
#define fx_owned_begin_path fx_test_nolink_owned_begin_path
#define fx_owned_begin_path_ephemeral fx_test_nolink_owned_begin_path_ephemeral
#define fx_owned_close fx_test_nolink_owned_close
#define fx_owned_copy_source fx_test_nolink_owned_copy_source
#define fx_owned_dispose fx_test_nolink_owned_dispose
#define fx_owned_fd fx_test_nolink_owned_fd
#define fx_owned_fd_size fx_test_nolink_owned_fd_size
#define fx_owned_file_info fx_test_nolink_owned_file_info
#define fx_owned_fill fx_test_nolink_owned_fill
#define fx_owned_finish fx_test_nolink_owned_finish
#define fx_owned_finish_ephemeral fx_test_nolink_owned_finish_ephemeral
#define fx_owned_freshen fx_test_nolink_owned_freshen
#define fx_owned_open_object fx_test_nolink_owned_open_object
#define fx_owned_open_store fx_test_nolink_owned_open_store
#define fx_owned_publish fx_test_nolink_owned_publish
#define fx_owned_read fx_test_nolink_owned_read
#define fx_owned_reject fx_test_nolink_owned_reject
#define fx_owned_same_entry fx_test_nolink_owned_same_entry
#define fx_owned_sync fx_test_nolink_owned_sync
#define fx_owned_write fx_test_nolink_owned_write
#define fx_batch_begin fx_test_nolink_batch_begin
#define fx_batch_count fx_test_nolink_batch_count
#define fx_batch_deferred fx_test_nolink_batch_deferred
#define fx_batch_open_file fx_test_nolink_batch_open_file
#define fx_batch_stage fx_test_nolink_batch_stage
#define fx_batch_write fx_test_nolink_batch_write
#define fx_batch_stage_end fx_test_nolink_batch_stage_end
#define fx_batch_commit fx_test_nolink_batch_commit
#define fx_batch_end fx_test_nolink_batch_end
#include "../../../src/proc/fx_immutable_owned.c"
int fx_test_nolink_attempts(void) { return attempts; }
