#include <stdlib.h>
extern void sainit (void);
extern void safinal (void);
extern void do_it_in_ada2(void);
int main (void) {
  sa3init();
  do_it_in_ada2 ();
  sa3final();
  exit (0);
}
