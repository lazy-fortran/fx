# Native Windows storage probe

Compile `probe.c` with the eight store C sources,
`test-support/fx_test_fs/src/fs_test_shim.c`, and `-ladvapi32`, using native
MinGW UCRT with `-std=c11 -Wall -Wextra -Werror`. Run the executable with a fresh
absolute fixture directory. It creates its own files and starts one child to
check that an independently opened owner record remains readable while its
advisory lock is held.

The oracle checks ordinary-root rejection without metadata writes, initialized
store namespaces, exclusive publication of known bytes, active lease protection,
release followed by GC, action-record publication, actual owner SID and DACL,
and idempotent locking. It also checks UTF-8 and long filenames, positional I/O,
reparse rejection, private ACL rejection, creation through a displaced parent,
and rejection of independently replaced tree staging. Original owned bytes are
checked before disposal; the replacement retains its known bytes afterward.
The native test support is exercised for locks, handle leaks, recursive cleanup,
temporary-root guards, and monotonic waits. Printed modes and GC counts retain
failure evidence.
The caller owns fixture cleanup after the process and its child have exited.

The tree-swap oracle closes its child file before renaming the directory.
Windows rejects renaming a directory that contains an open child file with
`ERROR_ACCESS_DENIED`, even when the child permits deletion sharing.

The implementation uses native Win32/NT file handles and UCRT file descriptors.
It requires Windows 10 or later with native extended rename/disposition support;
unsupported filesystem operations fail explicitly. Store algorithms, metadata,
and lease formats stay shared with Linux and macOS.
