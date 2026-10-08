# Native Windows storage probe

Compile `probe.c` with the eight store C sources and `-ladvapi32`, using native
MinGW UCRT with `-std=c11 -Wall -Wextra -Werror`. Run the executable with a fresh
absolute fixture directory. It creates its own files and starts one child to
check that an independently opened owner record remains readable while its
advisory lock is held.

The oracle checks ordinary-root rejection without metadata writes, initialized
store namespaces, exclusive publication of known bytes, active lease protection,
release followed by GC, action-record publication, actual owner SID and DACL,
and idempotent locking. Printed modes and GC counts retain failure evidence.
The caller owns fixture cleanup after the process and its child have exited.

The implementation uses native Win32/NT file handles and UCRT file descriptors.
It requires Windows 10 or later with native extended rename/disposition support;
unsupported filesystem operations fail explicitly. Store algorithms, metadata,
and lease formats stay shared with Linux and macOS.
