#include <stdint.h>
#include <stdio.h>

#define BIT(var, i) ((int)(!!((var) & (1 << (i)))))

#if 0
static void
cpuid(uint32_t which, uint32_t abcd[4])
{
    asm("cpuid;"
        "mov %%eax, (%0);"
        "mov %%ebx, 4(%0);"
        "mov %%ecx, 8(%0);"
        "mov %%edx, 12(%0);"
        :
        : "D"(abcd), "a"(which)
        : "%ebx", "%ecx", "%edx");
}
#endif

static int
cpuid(uint32_t which, uint32_t max, uint32_t *a, uint32_t *b, uint32_t *c,
    uint32_t *d)
{
    *a = *b = *c = *d = 0;
    if (which > max) {
        return 0;
    }
    asm("cpuid;"
        "mov %%eax, %0;"
        "mov %%ebx, %1;"
        "mov %%ecx, %2;"
        "mov %%edx, %3;"
        : "=m"(*a), "=m"(*b), "=m"(*c), "=m"(*d)
        : "a"(which)
        : "%ebx", "%ecx", "%edx");
    return 1;
}

int
main(void)
{
    uint32_t max, a, b, c, d;
    uint32_t name[4];

    cpuid(0, 0, &a, &b, &c, &d);
    max = a;
    name[0] = b;
    name[1] = d;
    name[2] = c;
    name[3] = 0;
    printf("%s\n", (char *)name);

    cpuid(1, max, &a, &b, &c, &d);
    printf("SSE1 = %d\n", BIT(d, 25));
    printf("SSE2 = %d\n", BIT(d, 26));
    printf("SSE3 = %d\n", BIT(c, 0));
    printf("SSSE3 = %d\n", BIT(c, 9));
    printf("PDCM = %d\n", BIT(c, 15));
    printf("AVX = %d\n", BIT(c, 28));
    printf("Hypervisor = %d\n", BIT(c, 31));

    cpuid(0x80000007, 0xffffffff, &a, &b, &c, &d);
    printf("%08lx %08lx %08lx %08lx\n", (unsigned long)a, (unsigned long)b,
        (unsigned long)c, (unsigned long)d);

    return 0;
}
