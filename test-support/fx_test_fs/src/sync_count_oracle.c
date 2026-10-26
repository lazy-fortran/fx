/* Test-only durability observer for batched blob publication.
 * Compiles the exact owned-capture implementation with isolated symbols and
 * counts every fsync/syncfs and every name link, so a test can check that a
 * batch costs a constant number of barriers and links no name before its
 * payload barrier. A barrier failure can be injected.
 */
#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
static int syncs, barriers, links, early_links, fail_barrier;
static int counted_fsync(int fd) { ++syncs; return fsync(fd); }
#ifdef __linux__
static int counted_syncfs(int fd)
{
    ++syncs;
    ++barriers;
    if (fail_barrier) { errno = EIO; return -1; }
    return syncfs(fd);
}
#define syncfs counted_syncfs
#endif
static int observed_linkat(int from, const char *source, int to,
                           const char *destination, int flags)
{
    ++links;
    if (barriers == 0) ++early_links;
    return linkat(from, source, to, destination, flags);
}
#define fsync counted_fsync
#define linkat observed_linkat
#define fx_owned_begin_at fx_test_sync_owned_begin_at
#define fx_owned_begin_path fx_test_sync_owned_begin_path
#define fx_owned_begin_path_ephemeral fx_test_sync_owned_begin_path_ephemeral
#define fx_owned_close fx_test_sync_owned_close
#define fx_owned_copy_source fx_test_sync_owned_copy_source
#define fx_owned_dispose fx_test_sync_owned_dispose
#define fx_owned_fd fx_test_sync_owned_fd
#define fx_owned_fd_size fx_test_sync_owned_fd_size
#define fx_owned_file_info fx_test_sync_owned_file_info
#define fx_owned_fill fx_test_sync_owned_fill
#define fx_owned_finish fx_test_sync_owned_finish
#define fx_owned_finish_ephemeral fx_test_sync_owned_finish_ephemeral
#define fx_owned_freshen fx_test_sync_owned_freshen
#define fx_owned_open_object fx_test_sync_owned_open_object
#define fx_owned_open_store fx_test_sync_owned_open_store
#define fx_owned_publish fx_test_sync_owned_publish
#define fx_owned_read fx_test_sync_owned_read
#define fx_owned_reject fx_test_sync_owned_reject
#define fx_owned_same_entry fx_test_sync_owned_same_entry
#define fx_owned_sync fx_test_sync_owned_sync
#define fx_owned_write fx_test_sync_owned_write
#define fx_batch_begin fx_test_sync_batch_begin
#define fx_batch_count fx_test_sync_batch_count
#define fx_batch_deferred fx_test_sync_batch_deferred
#define fx_batch_open_file fx_test_sync_batch_open_file
#define fx_batch_stage fx_test_sync_batch_stage
#define fx_batch_write fx_test_sync_batch_write
#define fx_batch_stage_end fx_test_sync_batch_stage_end
#define fx_batch_commit fx_test_sync_batch_commit
#define fx_batch_end fx_test_sync_batch_end
#include "../../../src/proc/fx_immutable_owned.c"
void fx_test_sync_reset(int fail)
{
    syncs = barriers = links = early_links = 0;
    fail_barrier = fail;
}
int fx_test_sync_calls(void) { return syncs; }
int fx_test_sync_links(void) { return links; }
int fx_test_sync_early_links(void) { return early_links; }
