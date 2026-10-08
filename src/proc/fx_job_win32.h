#ifndef FX_JOB_WIN32_H
#define FX_JOB_WIN32_H
#ifdef _WIN32
#include <windows.h>
/* Positive errno on failure; the caller owns the job handle. The opaque
   tracker retains exact member handles until signaled or explicitly freed.
   Drain terminates this job only and shares one bounded wait deadline. */
int fx_win32_job_tree_poll(HANDLE job, void **tracker);
int fx_win32_job_tree_drain(HANDLE job, void **tracker, DWORD timeout_ms);
void fx_win32_job_tree_free(void **tracker);
#endif
#endif
