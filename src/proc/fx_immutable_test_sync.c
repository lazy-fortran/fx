/* Bounded synchronization primitives; assertions and orchestration are Fortran. */
#define _POSIX_C_SOURCE 200809L
#define _DARWIN_C_SOURCE
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#define SYNC_PATH 4096
static int pause_phase, next_phase;
static char ready[SYNC_PATH], release[SYNC_PATH];
static char next_ready[SYNC_PATH], next_release[SYNC_PATH];
static void set_path(char *out, const char *value)
{
    size_t n = strlen(value);
    if (n >= SYNC_PATH) n = 0;
    memcpy(out, value, n);
    out[n] = '\0';
}
void fx_immutable_owned_test_configure(int phase, const char *first_ready,
                                     const char *first_release)
{
    pause_phase = phase;
    next_phase = 0;
    set_path(ready, first_ready);
    set_path(release, first_release);
}
void fx_immutable_owned_test_chain(int first, int second,
        const char *first_ready, const char *first_release,
        const char *second_ready, const char *second_release)
{
    fx_immutable_owned_test_configure(first, first_ready, first_release);
    next_phase = second;
    set_path(next_ready, second_ready);
    set_path(next_release, second_release);
}
void fx_immutable_owned_pause(int phase, const char *path)
{
    struct timespec delay = {0, 10000000};
    if (phase != pause_phase || !ready[0] || !path[0]) return;
    FILE *marker = fopen(ready, "w");
    if (!marker) return;
    fprintf(marker, "%s\n", path);
    if (fclose(marker) != 0) return;
    for (int i = 0; i < 3000; ++i) {
        if (access(release, F_OK) == 0) break;
        (void)nanosleep(&delay, NULL);
    }
    pause_phase = next_phase;
    next_phase = 0;
    set_path(ready, next_ready);
    set_path(release, next_release);
}
