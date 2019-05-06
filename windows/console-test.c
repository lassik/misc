#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <windows.h>

#define MAXEVENTS 16

static size_t width;
static size_t height;
static HANDLE cons;
static HANDLE consin;
static size_t ncell;
static CHAR_INFO *cells;
static COORD topleft;
static COORD botright;
static SMALL_RECT rect;
static CONSOLE_SCREEN_BUFFER_INFO consinfo;
static CONSOLE_CURSOR_INFO conscurs;

static char alphabet[] =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

static void die(const char *msg) {
    fprintf(stderr, "%s: %lu\n", msg, (unsigned long)GetLastError());
    exit(1);
}

void clearCell(CHAR_INFO *cell) { memset(cell, 0, sizeof(*cell)); }

void setWhiteOnBlack(CHAR_INFO *cell) {
    clearCell(cell);
    cell->Char.UnicodeChar = ' ';
    cell->Attributes = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
}

void randomizeCell(CHAR_INFO *cell) {
    clearCell(cell);
    cell->Char.AsciiChar = alphabet[rand() % strlen(alphabet)];
    cell->Attributes = rand() % 0xffff;
}

static void writeText(const char *s) {
    size_t x;

    for (x = 0; x < width; x++) {
        setWhiteOnBlack(&cells[x]);
        if (*s) {
            cells[x].Char.AsciiChar = *s++;
        }
    }
}

void allocate(size_t x, size_t y) {
    width = x;
    height = y;
    ncell = width * height;
    if (!(cells = realloc(cells, ncell * sizeof(*cells)))) {
        exit(1);
    }
    memset(cells, 0, ncell * sizeof(*cells));
}

static void reportMouse(size_t btn, size_t flags, size_t x, size_t y) {
    char buf[32];
    const char *btnstr = "";

    btnstr = "";
    if (flags & MOUSE_WHEELED) {
        if ((short)(HIWORD(btn)) > 0) {
            btnstr = "wheelup";
        } else {
            btnstr = "wheeldown";
        }
    } else {
        switch (btn) {
        case FROM_LEFT_1ST_BUTTON_PRESSED:
            btnstr = "left";
            break;
        case FROM_LEFT_2ND_BUTTON_PRESSED:
            btnstr = "middle";
            break;
        case RIGHTMOST_BUTTON_PRESSED:
            btnstr = "right";
            break;
        }
    }
    snprintf(buf, sizeof(buf), "%zu %zu %s", x, y, btnstr);
    writeText(buf);
}

static const char *keyToString(unsigned int key) {
    switch (key) {
    case VK_LEFT:
        return "left";
    case VK_RIGHT:
        return "right";
    case VK_UP:
        return "up";
    case VK_DOWN:
        return "down";
    case VK_INSERT:
        return "insert";
    case VK_DELETE:
        return "delete";
    }
    return 0;
}

static void onKeyDown(unsigned int key) {
    const char *s;

    s = keyToString(key);
    if (!s) {
        s = "unknown key";
    }
    writeText(s);
}

static void onKeyChar(unsigned long unichar) {
    char buf[16];

    snprintf(buf, sizeof(buf), "U+%04X", unichar);
    writeText(buf);
}

int main(void) {
    static INPUT_RECORD events[MAXEVENTS];
    INPUT_RECORD *event;
    DWORD nevent;
    // COORD winsize = {100, 100};
    size_t x, y, i;

    consin = GetStdHandle(STD_INPUT_HANDLE);
    cons = GetStdHandle(STD_OUTPUT_HANDLE);
    // SetConsoleScreenBufferSize(cons, winsize);
    if (!GetConsoleScreenBufferInfo(cons, &consinfo)) {
        die("GetConsoleScreenBufferInfo");
    }
    allocate(consinfo.dwSize.X, consinfo.dwSize.Y);
    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            randomizeCell(&cells[y * width + x]);
        }
    }
    writeText("Hello world");
    for (;;) {
        botright.X = width - 1;
        botright.Y = height - 1;
        rect.Right = width - 1;
        rect.Bottom = height - 1;
        if (!WriteConsoleOutput(cons, cells, botright, topleft, &rect)) {
            die("WriteConsoleOutput");
        }
        if (!GetConsoleCursorInfo(cons, &conscurs)) {
            die("GetConsoleCursorInfo");
        }
        conscurs.bVisible = 0;
        if (!SetConsoleCursorInfo(cons, &conscurs)) {
            die("SetConsoleCursorInfo");
        }
        if (!ReadConsoleInput(consin, events, MAXEVENTS, &nevent)) {
            die("ReadConsoleInput");
        }
        for (i = 0; i < nevent; i++) {
            event = &events[i];
            switch (event->EventType) {
            case KEY_EVENT:

                if (event->Event.KeyEvent.bKeyDown) {
                    if (event->Event.KeyEvent.uChar.UnicodeChar) {
                        onKeyChar(event->Event.KeyEvent.uChar.UnicodeChar);
                    } else {
                        onKeyDown(event->Event.KeyEvent.wVirtualKeyCode);
                    }
                }
                // event->Event.KeyEvent.uChar.UnicodeChar;
                break;
            case MOUSE_EVENT:
                reportMouse(event->Event.MouseEvent.dwButtonState,
                            event->Event.MouseEvent.dwEventFlags,
                            event->Event.MouseEvent.dwMousePosition.X,
                            event->Event.MouseEvent.dwMousePosition.Y);
                break;
            case WINDOW_BUFFER_SIZE_EVENT:
                allocate(event->Event.WindowBufferSizeEvent.dwSize.X,
                         event->Event.WindowBufferSizeEvent.dwSize.Y);
                break;
            }
        }
    }
    return 0;
}
