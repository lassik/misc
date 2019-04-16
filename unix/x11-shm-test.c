#include <sys/types.h>

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <X11/Xlib.h>

#define WIDTH 300
#define HEIGHT 200

#define NANOSECONDS (500 * 1000)

static void
die(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static void
diesys(const char *msg)
{
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
    exit(1);
}

static int
allocate_shared_memory(size_t size)
{
    char name[16];
    int fd, flags;

    snprintf(name, sizeof(name), "/%lu", (unsigned long)getpid());
    if ((fd = shm_open(name, O_RDWR | O_CREAT | O_EXCL, 0600)) == -1) {
        diesys("shm_open");
    }
    if (shm_unlink(name) == -1) {
        diesys("shm_unlink");
    }
    if ((flags = fcntl(fd, F_GETFD)) == -1) {
        diesys("fcntl F_GETFD");
    }
    flags &= ~FD_CLOEXEC;
    if (fcntl(fd, F_SETFD, flags) == -1) {
        diesys("fcntl F_SETFD");
    }
    if (ftruncate(fd, (off_t)size) == -1) {
        diesys("ftruncate");
    }
    return fd;
}

static void *
map_shared_memory_from_fd(int fd, size_t *out_size)
{
    struct stat st;
    void *buf;
    size_t size;

    if (fstat(fd, &st) == -1) {
        diesys("fstat");
    }
    *out_size = size = (size_t)st.st_size;
    buf = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (buf == MAP_FAILED) {
        diesys("mmap");
    }
    return buf;
}

static void
parent_x11_event_loop(uint32_t *pixels)
{
    static struct timespec tv;
    Display *display;
    int screen;
    XSetWindowAttributes winattr;
    Window window;
    GC gc;
    XGCValues gcvalues;
    Visual *visual = 0;
    XImage *image;
    XEvent event, fakeExpose;
    int shouldquit;

    tv.tv_sec = 0;
    tv.tv_nsec = NANOSECONDS;
    if (!(display = XOpenDisplay(0))) {
        die("cannot open X display");
    }
    screen = DefaultScreen(display);
    winattr.event_mask = ExposureMask | KeyPressMask;
    window = XCreateWindow(display, XDefaultRootWindow(display), 0, 0, WIDTH,
        HEIGHT, 0, DisplayPlanes(display, screen), InputOutput, visual,
        CWBorderPixel | CWEventMask, &winattr);
    window = XCreateSimpleWindow(display, XDefaultRootWindow(display), 0, 0,
        WIDTH, HEIGHT, 0, XWhitePixel(display, screen),
        XBlackPixel(display, screen));
    XStoreName(display, window, "X11 shared memory test");
    XSelectInput(display, window, ExposureMask | KeyPressMask);
    XMapWindow(display, window);
    gc = XCreateGC(display, window, 0, &gcvalues);
    image = XCreateImage(display, visual, DisplayPlanes(display, screen),
        ZPixmap, 0, (char *)pixels, WIDTH, HEIGHT, 32, 0);
    memset(&fakeExpose, 0, sizeof(fakeExpose));
    fakeExpose.xexpose.type = Expose;
    fakeExpose.xexpose.display = display;
    fakeExpose.xexpose.window = window;
    fakeExpose.xexpose.x = 0;
    fakeExpose.xexpose.y = 0;
    fakeExpose.xexpose.width = WIDTH;
    fakeExpose.xexpose.height = HEIGHT;
    shouldquit = 0;
    while (!shouldquit) {
        nanosleep(&tv, 0);
        static struct pollfd pollfd;
        pollfd.fd = ConnectionNumber(display);
        pollfd.events = POLLIN;
        if (poll(&pollfd, 1, 0) < 1) {
            XSendEvent(display, window, 0, ExposureMask, &fakeExpose);
        }
        XNextEvent(display, &event);
        switch (event.type) {
        case Expose:
            XPutImage(display, window, gc, image, 0, 0, 0, 0, WIDTH, HEIGHT);
            break;
        case KeyPress:
            shouldquit = 1;
            break;
        }
    }
}

static void
parent(void)
{
    char *child_args[3] = {"./x11-shm-test", "child", 0};
    uint32_t *buf;
    pid_t child;
    size_t bufsize;
    int fd, status;

    fd = allocate_shared_memory(WIDTH * HEIGHT * sizeof(uint32_t));
    buf = map_shared_memory_from_fd(3, &bufsize);
    memset(buf, 0, bufsize);
    if ((child = fork()) == -1) {
        diesys("fork");
    }
    if (!child) {
        if (dup2(fd, 3) == -1) {
            diesys("dup2");
        }
        execv(child_args[0], child_args);
        diesys("execv");
    }
    parent_x11_event_loop(buf);
    if (kill(child, SIGTERM) == -1) {
        diesys("cannot send SIGTERM to child");
    }
    if (waitpid(child, &status, 0) == -1) {
        diesys("cannot wait for child");
    }
}

static void
child(void)
{
    uint32_t *pixels;
    size_t nbyte;

    pixels = map_shared_memory_from_fd(3, &nbyte);
    if (nbyte < WIDTH * HEIGHT * sizeof(uint32_t)) {
        die("child got a too-small buffer for some reason");
    }
    for (;;) {
        struct timespec tv;
        uint32_t *p;
        uint32_t t;
        uint32_t r, g, b;
        size_t x, y;
        clock_gettime(CLOCK_REALTIME, &tv);
        t = tv.tv_nsec / NANOSECONDS;
        p = pixels;
        for (y = 0; y < HEIGHT; y++) {
            for (x = 0; x < WIDTH; x++) {
                r = ((x ^ y) & t) % 255;
                g = ((x & y) & t) % 255;
                b = ((x ^ y) & t) % 255;
                *p++ = (r << 16) | (g << 8) | b;
            }
        }
    }
}

int
main(int argc, char **argv)
{
    (void)argv;
    if (argc == 1) {
        parent();
    } else if (argc == 2) {
        child();
    } else {
        fprintf(stderr, "usage\n");
        exit(1);
    }
    return 0;
}
