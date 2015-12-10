#include <stdlib.h>
extern void sainit (void);
extern void safinal (void);
extern void do_it_in_ada(void);
int main (void) {
  sainit();
  do_it_in_ada ();
  safinal();
  exit (0);
}


