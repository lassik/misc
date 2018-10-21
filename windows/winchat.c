// Winsock hello world.
//
// TCP connect to localhost:1234 and show all text sent by the server
// in a window.

#define _UNICODE
#define UNICODE
#include <windows.h>

#define WM_USER_SOCKET WM_USER + 1
#define ID_EDITBOX 1000

static const wchar_t windowClassName[] = L"myWindowClass";
static WNDCLASSEX windowClass;
static HWND window;
static HWND editbox;
static SOCKET sock;
static char buf[4096];
static size_t buflen;
static WSADATA wsaData;
static SOCKADDR_IN sockaddr;

static void
die(const wchar_t *msg)
{
    MessageBox(window, msg, L"Error", MB_ICONEXCLAMATION | MB_OK);
    ExitProcess(0);
}

static void
dieSys(int errCode)
{
    wchar_t *msg;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
        0, errCode, 0, (LPTSTR)&msg, 0, 0);
    die(msg);
}

static void
onSocketRead(void)
{
    int n = recv(sock, buf + buflen, sizeof(buf) - buflen - 1, 0);
    if (n == SOCKET_ERROR) {
        dieSys(WSAGetLastError());
    }
    if (n < 1) {
        die(L"Nothing received");
    }
    buflen += n;
    SetWindowTextA(editbox, buf);
}

static void
onSocketClose(void)
{
    MessageBox(window, L"Client closed connection", L"Connection closed!",
        MB_ICONINFORMATION | MB_OK);
}

static void
onSocket(LPARAM lParam)
{
    switch (WSAGETSELECTEVENT(lParam)) {
    case FD_READ:
        onSocketRead();
        break;
    case FD_CLOSE:
        onSocketClose();
        break;
    }
}

static void
onWindowCreate(void)
{
    editbox = CreateWindowEx(0, L"EDIT", 0,
        WS_CHILD | WS_VISIBLE | WS_VSCROLL | ES_LEFT | ES_MULTILINE |
            ES_AUTOVSCROLL,
        0, 0, 0, 0, window, (HMENU)ID_EDITBOX,
        (HINSTANCE)GetWindowLong(window, GWL_HINSTANCE), 0);
    if (!editbox) {
        die(L"Create editbox");
    }
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        die(L"Winsock initialization failed");
    }
    sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock == INVALID_SOCKET) {
        die(L"Socket creation failed");
    }
    sockaddr.sin_port = htons(1234);
    sockaddr.sin_family = AF_INET;
    sockaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (connect(sock, (LPSOCKADDR)&sockaddr, sizeof(sockaddr)) ==
        SOCKET_ERROR) {
        dieSys(WSAGetLastError());
    }
    if (WSAAsyncSelect(sock, window, WM_USER_SOCKET, FD_READ | FD_CLOSE) != 0) {
        die(L"WSAAsyncSelect failed");
    }
}

static void
onWindowSize(void)
{
    RECT rect;

    GetClientRect(window, &rect);
    MoveWindow(editbox, 0, 0, rect.right, rect.bottom, 1);
}

static LRESULT CALLBACK
windowProcedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
    case WM_CREATE:
        window = hwnd;
        onWindowCreate();
        break;
    case WM_SIZE:
        onWindowSize();
        break;
    case WM_USER_SOCKET:
        onSocket(lParam);
        break;
    case WM_CLOSE:
        DestroyWindow(hwnd);
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    default:
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    return 0;
}

static void
createWindowClass(void)
{
    windowClass.cbSize = sizeof(windowClass);
    windowClass.style = 0;
    windowClass.lpfnWndProc = windowProcedure;
    windowClass.cbClsExtra = 0;
    windowClass.cbWndExtra = 0;
    windowClass.hInstance = GetModuleHandle(0);
    windowClass.hIcon = LoadIcon(0, IDI_APPLICATION);
    windowClass.hCursor = LoadCursor(0, IDC_ARROW);
    windowClass.hbrBackground = (HBRUSH)(COLOR_WINDOW);
    windowClass.lpszMenuName = 0;
    windowClass.lpszClassName = windowClassName;
    windowClass.hIconSm = LoadIcon(0, IDI_APPLICATION);
    if (!RegisterClassEx(&windowClass)) {
        die(L"RegisterClassEx");
    }
}

static void
createWindow(void)
{
    CreateWindowEx(WS_EX_CLIENTEDGE, windowClassName, L"The title of my window",
        WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, 240, 120, 0, 0,
        GetModuleHandle(0), 0);
    if (!window) {
        die(L"CreateWindowEx");
    }
}

int WINAPI
WinMain(
    HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    MSG msg;

    createWindowClass();
    createWindow();
    ShowWindow(window, nCmdShow);
    UpdateWindow(window);
    while (GetMessage(&msg, 0, 0, 0) > 0) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return msg.wParam;
}
