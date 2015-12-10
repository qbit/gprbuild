#include "cpp_classes.h"

extern "C" {
  void adainit (void); 
  void adafinal (void);
  void method1 (A *t);
  void proc();
}

void method1 (A *t)
{
  t->method1 ();
}

int main ()
{

  adainit();
  A obj;

  obj.method2 (3030);
  proc();
  adafinal ();
}

