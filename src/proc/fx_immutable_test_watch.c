/* Native file event primitive for the immutable reuse behavioral oracle. */
#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#ifdef __APPLE__
#include <sys/event.h>
static int watched_file = -1;
int fx_immutable_test_watch_open(const char *path)
{
    struct kevent event;
    int queue = kqueue();
    if (queue < 0) return -1;
    watched_file = open(path, O_RDONLY | O_NOFOLLOW);
    if (watched_file < 0) { close(queue); return -1; }
    EV_SET(&event, watched_file, EVFILT_VNODE, EV_ADD | EV_CLEAR,
           NOTE_WRITE | NOTE_ATTRIB | NOTE_DELETE | NOTE_RENAME, 0, NULL);
    if (kevent(queue, &event, 1, NULL, 0, NULL) < 0) {
        close(watched_file); watched_file = -1; close(queue); return -1;
    }
    return queue;
}
int fx_immutable_test_watch_poll(int queue)
{
    struct kevent event;
    struct timespec timeout = {0, 0};
    return kevent(queue, NULL, 0, &event, 1, &timeout);
}
void fx_immutable_test_watch_close(int queue)
{
    close(queue);
    if (watched_file >= 0) close(watched_file);
    watched_file = -1;
}
#else
#include <sys/inotify.h>
int fx_immutable_test_watch_open(const char *path)
{
    int queue = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    if (queue < 0) return -1;
    if (inotify_add_watch(queue, path, IN_MODIFY | IN_ATTRIB | IN_CLOSE_WRITE |
                          IN_DELETE_SELF | IN_MOVE_SELF) < 0) {
        close(queue); return -1;
    }
    return queue;
}
int fx_immutable_test_watch_poll(int queue)
{
    char events[4096];
    ssize_t count = read(queue, events, sizeof(events));
    if (count < 0 && errno == EAGAIN) return 0;
    return count < 0 ? -1 : count > 0;
}
void fx_immutable_test_watch_close(int queue) { close(queue); }
#endif
