/*
 * These are two classic sorting algorithms, insertion sort and
 * selection sort, augmented so that duplicate items are removed
 * during the sort. This is much simpler than removing them in a
 * separate pass.
 *
 * Selection sort is a natural fit for this task because at each
 * iteration it finds the smallest remaining item. This means that
 * duplicate items can simply be dropped and they will be overwritten
 * by new items on later iterations. When we are done we can simply
 * cut off the remainder of the array after the last non-duplicate
 * item.
 *
 * With insertion sort we remove duplicate items by changing them into
 * negative infinity (here -1) so that they all pile up at the
 * beginning of the array once the sort is done. Then we cut off that
 * part of the array. This is not as handy as cutting off the end of
 * the array since it changes the base address of the resulting array.
 *
 * A further benefit of selection sort is that the sorted items can be
 * consumed in order while the sort is running, since it always finds
 * the smallest item.
 *
 * Both insertion sort and selection sort are O(n**2) but work great
 * for small arrays and are among the simplest to implement.
 */

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
