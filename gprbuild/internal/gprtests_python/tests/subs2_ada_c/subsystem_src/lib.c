#include <stdio.h>
#include "lib.h"

void do_something (void) {
#ifdef XXX
  printf ("error: XXX defined\n");
#endif
#ifdef YYY
  printf ("error: YYY defined\n");
#endif
    printf ("Doing something in C \n");
}
