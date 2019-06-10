// gcc -c sbsc.c

#include <stdatomic.h>

void P0(atomic_int *x, atomic_int *y, int *t0a)
{
   atomic_store_explicit(x, 1, memory_order_relaxed);
   *t0a = atomic_load_explicit(y, memory_order_relaxed);
}

void P1(atomic_int *x, atomic_int *y, int *t1b)
{
   atomic_store_explicit(y, 1, memory_order_relaxed);
   *t1b = atomic_load_explicit(x, memory_order_relaxed);
}
