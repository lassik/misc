// Whenever you click within a top-level window or any of its child
// windows, this program assigns keyboard focus to that top-level
// window.  More aggressive than separate keyboard and mouse focus,
// but less aggressive than sloppy focus or focus follows mouse.
//
// I wrote this for use with the ratpoison window manager, which
// doesn't have this functionality, but I anticipate that it will work
// with any window manager that doesn't interfere with the grabbing
// and focusing stuff in ways that I am too ignorant to enumerate.
//
// gcc -Wall -pedantic -std=c99 -g -o x-click-to-focus x-click-to-focus.c -I /usr/X11R6/include -L /usr/X11R6/lib -l X11

#include <err.h>
#include <stdio.h>
#include <stdlib.h>

#include <X11/Xlib.h>

static Display *disp_var;

static Display *disp(void) {
    if(!disp_var) {
        disp_var = XOpenDisplay(0);
        if(!disp_var) errx(1, "cannot open X display %s", XDisplayName(0));
    }
    return(disp_var);
}

static Window win_top_level_parent(Window win) {
    Window root;
    Window parent;
    Window *childv;
    unsigned int nchild;
    for(;;) {
        if(!win) return(0);
        if(!XQueryTree(disp(), win, &root, &parent, &childv, &nchild)) return(0);
        if(childv) XFree(childv);
        if(root == win) return(0);
        if(root == parent) return(win);
        win = parent;
    }
}

static void focus_top_level_parent(Window win) {
    win = win_top_level_parent(win);
    if(win) XSetInputFocus(disp(), win, RevertToParent, CurrentTime);
}

static void focus_mouse_top_level_parent(XButtonEvent *xbutton) {
    focus_top_level_parent(xbutton->subwindow);
}

static void spy_mouse_button_presses(void (*fun)(XButtonEvent *)) {
    XEvent event_mem;
    XEvent *event = &event_mem;
    XGrabButton(disp(),
                AnyButton,
                AnyModifier,
                XDefaultRootWindow(disp()),
                False,
                ButtonPressMask,
                GrabModeSync,
                GrabModeAsync,
                None,
                None);
    // We never ungrab the pointer.  I rely on the X server being
    // smart enough to ungrab the pointer by itself if the X
    // connection to the program grabbing the pointer dies.
    for(;;) {
        XNextEvent(disp(), event);
        if(event->type == ButtonPress) {
            fun(&event->xbutton);
            XAllowEvents(disp(), ReplayPointer, CurrentTime);
        }
    }
}

int main(void) {
    spy_mouse_button_presses(focus_mouse_top_level_parent);
    return(0);
}
