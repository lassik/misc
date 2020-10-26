#include <stdio.h>
#include <stdlib.h>

enum { n = 8 };

static char board[n * n];

static int
linear_from_yx(int y, int x)
{
    return (y * n) + x;
}

static void
show_board(void)
{
    int y;
    int x;

    for (y = 0; y < n; ++y) {
        for (x = 0; x < n; ++x) {
            printf("%c", board[linear_from_yx(y, x)] ? '#' : '.');
        }
        printf("\n");
    }
}

static void
solved(void)
{
    show_board();
    exit(0);
}

static int
straight(int y, int x)
{
    int i;

    for (i = 0; i < n; ++i) {
        if (board[linear_from_yx(i, x)]) {
            return 0;
        }
        if (board[linear_from_yx(y, i)]) {
            return 0;
        }
    }
    return 1;
}

static int
diagonal(int y, int x, int dy, int dx)
{
    while ((y > 0) && (y < n) && (x > 0) && (x < n)) {
        y += dy;
        x += dx;
    }
    do {
        if (board[linear_from_yx(y, x)]) {
            return 0;
        }
        y -= dy;
        x -= dx;
    } while ((y > 0) && (y < n) && (x > 0) && (x < n));
    return 1;
}

static int
queen_safe_at(int y, int x)
{
    return straight(y, x) && diagonal(y, x, -1, -1) && diagonal(y, x, -1, 1);
}

static void
queens(int count)
{
    int y;
    int x;

    if (count < 1) {
        solved();
    } else {
        for (y = 0; y < n; ++y) {
            for (x = 0; x < n; ++x) {
                if (queen_safe_at(y, x)) {
                    board[linear_from_yx(y, x)] = 1;
                    queens(count - 1);
                    board[linear_from_yx(y, x)] = 0;
                }
            }
        }
    }
}

int
main(void)
{
    queens(n);
    return 0;
}
