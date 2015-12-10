#include <stdlib.h>
extern void asainit (void);
extern void asafinal (void);
extern void do_it_in_ada(void);
int main (void) {
  asainit();
  do_it_in_ada ();
  asafinal();
  exit (0);
}
