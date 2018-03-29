#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int insertion_sort_uniq(int *a, int n, int **out_a) {
  int i, gap, tmp;
  int ndup, isdup;

  ndup = 0;
  for (i = 1; i < n; i++) {
    gap = i;
    tmp = a[gap];
    isdup = 0;
    while (gap > 0 && tmp <= a[gap - 1]) {
      if (tmp == a[gap - 1]) {
        /* Remove duplicate */
        tmp = -1;
        isdup = 1;
      }
      a[gap] = a[gap - 1];
      gap--;
    }
    a[gap] = tmp;
    if (isdup)
      ndup++;
  }
  *out_a = a + ndup;
  n -= ndup;
  return n;
}

static int selection_sort_uniq(int *a, int n) {
  int tail, mini, m, i, old;

  for (i = tail = 0; tail < n; tail++) {
    for (mini = m = tail; m < n; m++) {
      if (a[mini] > a[m]) {
        mini = m;
      }
    }
    old = a[tail];
    a[tail] = a[mini];
    a[mini] = old;
    if ((i > 0) && (a[i - 1] == a[tail])) {
      /* Remove duplicate */
      continue;
    }
    a[i] = a[tail];
    /* We could print a[i] here already, if we wanted to. */
    i++;
  }
  return i;
}

static void print(int *a, int n) {
  int i;

  for (i = 0; i < n; i++) {
    printf("%d ", a[i]);
  }
  printf("\n");
}

static void usage(void) {
  fprintf(stderr, "usage: sort-uniq insertion|selection\n");
  exit(1);
}

int main(int argc, char **argv) {
  int a[] = {5, 2, 7, 5, 3, 2, 5, 7, 9, 8, 4, 1, 6, 6, 5};
  int *b = 0;
  int n = sizeof(a) / sizeof(a[0]);

  if (argc != 2) {
    usage();
  }
  if (!strcmp(argv[1], "insertion")) {
    n = insertion_sort_uniq(a, n, &b);
  } else if (!strcmp(argv[1], "selection")) {
    n = selection_sort_uniq(a, n);
    b = a;
  } else {
    usage();
  }
  print(b, n);
  return 0;
}
