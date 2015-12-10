#include <stdio.h>

#if defined (_WIN32)
/* On windows, we need to tell the compiler that sub() is imported
   from a DLL.  */
extern _stdcall int sub ();
#else
extern int sub ();
#endif

main ()
{
  int a = 0;

  a = sub ();

  printf ("%d\n", a);
}

