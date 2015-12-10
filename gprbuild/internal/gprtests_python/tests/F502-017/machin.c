#include "machin.h"

typedef struct
{
   unsigned short a1;
#ifdef VMS
   unsigned a2;
#else
   unsigned long  a2;
#endif
} structure_folle;


void fonction_bidon()
{
  structure_folle v_Sstruct;

  float v_float_value = 123.456;

  int size_de_float = 0;

   v_Sstruct.a1 = 12;
   v_Sstruct.a2 = 34;

  size_de_float = sizeof(v_float_value);

   fonction_ada_float(v_float_value);
   fonction_ada(v_Sstruct);



}
