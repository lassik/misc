#ifndef ALIGN_MAX_H
#define ALIGN_MAX_H

#if __STDC_VERSION__ >= 199901L
#include <stdint.h>
typedef union {
    void *a;
    intmax_t b;
    long double c;
} align_max_t;
#else
typedef union {
    void *a;
    long b;
    double c;
} align_max_t;
#endif

#define ALIGN_MAX sizeof(align_max_t)

#endif
