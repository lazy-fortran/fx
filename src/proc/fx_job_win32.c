#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include "fx_job_win32.h"
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>

struct job_member { HANDLE process; DWORD pid; uint64_t birth; };
struct job_tree { struct job_member *members; size_t count, capacity; };

static int job_error(DWORD error) {
    switch (error) {
    case ERROR_ACCESS_DENIED: case ERROR_PRIVILEGE_NOT_HELD: return EACCES;
    case ERROR_NOT_ENOUGH_MEMORY: case ERROR_OUTOFMEMORY: return ENOMEM;
    case ERROR_INVALID_HANDLE: return EBADF;
    case ERROR_INVALID_PARAMETER: return EINVAL;
    default: return EIO;
    }
}
static int prune_members(struct job_tree *tree) {
    size_t slot = 0;
    while (slot < tree->count) {
        DWORD state = WaitForSingleObject(tree->members[slot].process, 0);
        if (state == WAIT_FAILED) return job_error(GetLastError());
        if (state == WAIT_OBJECT_0) {
            CloseHandle(tree->members[slot].process);
            tree->members[slot] = tree->members[--tree->count];
        } else ++slot;
    }
    return 0;
}
int fx_win32_job_tree_poll(HANDLE job, void **tracker) {
    if (!job || job == INVALID_HANDLE_VALUE || !tracker) return EINVAL;
    if (!*tracker) {
        *tracker = calloc(1, sizeof(struct job_tree));
        if (!*tracker) return ENOMEM;
    }
    struct job_tree *tree = *tracker;
    int error = prune_members(tree);
    if (error) return error;
    size_t capacity = 32;
    JOBOBJECT_BASIC_PROCESS_ID_LIST *list = NULL;
    for (;;) {
        size_t size = sizeof(*list) + capacity * sizeof(ULONG_PTR);
        list = calloc(1, size);
        if (!list) return ENOMEM;
        if (QueryInformationJobObject(job, JobObjectBasicProcessIdList, list, (DWORD)size, NULL)) break;
        DWORD failure = GetLastError();
        free(list); list = NULL;
        if (failure != ERROR_MORE_DATA) return job_error(failure);
        if (capacity >= 65536) return EOVERFLOW;
        capacity *= 2;
    }
    for (DWORD index = 0; index < list->NumberOfProcessIdsInList; ++index) {
        DWORD pid = (DWORD)list->ProcessIdList[index];
        HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, pid);
        if (!process) {
            DWORD failure = GetLastError();
            if (failure == ERROR_INVALID_PARAMETER) continue; /* Already gone. */
            error = job_error(failure); break;
        }
        BOOL owned = FALSE;
        if (!IsProcessInJob(process, job, &owned)) error = job_error(GetLastError());
        FILETIME created, exited, kernel, user;
        if (!error && owned && !GetProcessTimes(process, &created, &exited, &kernel, &user))
            error = job_error(GetLastError());
        if (error || !owned) { CloseHandle(process); if (error) break; continue; }
        uint64_t birth = ((uint64_t)created.dwHighDateTime << 32) | created.dwLowDateTime;
        size_t slot;
        for (slot = 0; slot < tree->count; ++slot)
            if (tree->members[slot].pid == pid && tree->members[slot].birth == birth) break;
        if (slot < tree->count) { CloseHandle(process); continue; }
        if (tree->count == tree->capacity) {
            size_t count = tree->capacity ? tree->capacity * 2 : 32;
            struct job_member *grown = realloc(tree->members, count * sizeof(*grown));
            if (!grown) { CloseHandle(process); error = ENOMEM; break; }
            tree->members = grown; tree->capacity = count;
        }
        tree->members[tree->count++] = (struct job_member){process, pid, birth};
    }
    free(list); return error;
}
int fx_win32_job_tree_drain(HANDLE job, void **tracker, DWORD timeout_ms) {
    ULONGLONG deadline = GetTickCount64() + timeout_ms;
    int error = fx_win32_job_tree_poll(job, tracker);
    if (error) return error;
    if (!TerminateJobObject(job, 124)) return job_error(GetLastError());
    struct job_tree *tree = *tracker;
    /* ActiveProcesses can become zero before the process handles signal.
       Wait each exact captured handle as well as the final job accounting. */
    for (size_t slot = 0; slot < tree->count; ++slot) {
        ULONGLONG now = GetTickCount64();
        DWORD wait = WaitForSingleObject(tree->members[slot].process,
            now < deadline ? (DWORD)(deadline - now) : 0);
        if (wait != WAIT_OBJECT_0)
            return wait == WAIT_TIMEOUT ? ETIMEDOUT : job_error(GetLastError());
    }
    for (;;) {
        JOBOBJECT_BASIC_ACCOUNTING_INFORMATION information;
        if (!QueryInformationJobObject(job, JobObjectBasicAccountingInformation,
            &information, sizeof(information), NULL)) return job_error(GetLastError());
        if (!information.ActiveProcesses) return prune_members(tree);
        if (GetTickCount64() >= deadline) return ETIMEDOUT;
        Sleep(10);
    }
}
void fx_win32_job_tree_free(void **tracker) {
    if (!tracker || !*tracker) return;
    struct job_tree *tree = *tracker;
    for (size_t slot = 0; slot < tree->count; ++slot) CloseHandle(tree->members[slot].process);
    free(tree->members); free(tree); *tracker = NULL;
}
#endif
