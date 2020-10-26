#include <X11/Xlib.h>
#include <X11/cursorfont.h>

#include <string.h>

static Display *disp;

static Cursor
new_invisible_cursor(void)
{
    XColor nullc;
    Pixmap nullp;

    memset(&nullc, 0, sizeof(XColor));
    nullp = XCreatePixmap(disp, XDefaultRootWindow(disp), 1, 1, 1);
    return XCreatePixmapCursor(disp, nullp, nullp, &nullc, &nullc, 0, 0);
}

static void
win_tree_each(Window w, void (*fun)(Window))
{
    Window root;
    Window parent;
    Window *childv;
    unsigned int nchild;

    fun(w);
    if (XQueryTree(disp, w, &root, &parent, &childv, &nchild)) {
        while (nchild) {
            win_tree_each(childv[--nchild], fun);
        }
        if (childv) {
            XFree(childv);
        }
    }
}

static void
win_spy_keys(Window w)
{
    XSelectInput(disp, w, KeyPressMask);
}

int
main(void)
{
    Cursor c;
    XEvent e;
    int grabbed;

    disp = XOpenDisplay(0);
    c = new_invisible_cursor();
    win_tree_each(XDefaultRootWindow(disp), win_spy_keys);
    grabbed = 0;
    for (;;) {
        XNextEvent(disp, &e);
        if (!grabbed && (e.type == KeyPress)) {
            grabbed = 1;
            XGrabPointer(disp, XDefaultRootWindow(disp), True,
                PointerMotionMask, GrabModeAsync, GrabModeAsync, None, c,
                CurrentTime);
        }
        if (grabbed && (e.type == MotionNotify)) {
            XUngrabPointer(disp, CurrentTime);
            grabbed = 0;
        }
    }
    return 0;
}
