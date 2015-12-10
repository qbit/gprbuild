#include <stdlib.h>
extern void sa3init (void);
extern void sa3final (void);
extern void do_it_in_ada(void);
int main (void) {
  sa3init();
  do_it_in_ada ();
  sa3final();
  exit (0);
}


