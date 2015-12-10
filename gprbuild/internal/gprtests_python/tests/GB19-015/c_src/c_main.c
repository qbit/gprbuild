extern void adainit (void);
extern void adafinal (void);
extern void do_it_in_ada(void);
#include <stdio.h>
#include <stdlib.h>
#include "lib.h"

int main (void) {
  adainit();

#ifdef XXX
  printf ("XXX defined\n");
#endif
#ifdef YYY
  printf ("YYY defined\n");
#endif

  do_it_in_ada ();
  do_something ();
  adafinal();
  exit (0);
}


