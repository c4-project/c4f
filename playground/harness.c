// gcc sbsc.o harness.c
// ./a.out

#include <stdatomic.h>
#include <stdio.h>

void P0(atomic_int *x, atomic_int *y, int *t0a);
void P1(atomic_int *x, atomic_int *y, int *t1b);

int main () {

  atomic_int x[10];
  atomic_int y[10];
  
  for (int i = 0; i<10; i++) {

    x[i] = 0;
    y[i] = 0;
    int t0a = 0;
    int t1b = 0;
    
    P0(&x[i], &y[i], &t0a);
    
    P1(&x[i], &y[i], &t1b);
    
    printf ("t0a=%d, t1b=%d.\n", t0a, t1b);
  }

  return 0;

}
