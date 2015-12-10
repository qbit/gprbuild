#include <stdio.h>
#include "svml_interface.h"

int main(int argc, char ** argv)
{
  double in[PATH_LENGTH];
  double out[PATH_LENGTH];
  double out2[PATH_LENGTH];
  unsigned i = 0;

  in[0] = 0.25456;
  in[1] = 0.14514;
  in[2] = 0.85956;
  in[3] = 0.45474;


  printf("\nvexp : \n");  
  vexp(in, out);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out[i]);

    }

  printf("\nvsin : \n");  

  vsin(in, out);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out[i]);

    }


  printf("\nvcos : \n");  

  vcos(in, out);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out[i]);

    }

  printf("\nvlog : \n");  

  vlog(in, out);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out[i]);

    }


  printf("\nvsqrt : \n");  

  vsqrt(in, out);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out[i]);

    }

  printf("\nvfloor : \n");  
  vfloor(in, out);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out[i]);

    }

  out[0] = 1;
  out[1] = 2;
  out[2] = 4.85956;
  out[3] = 4;

  printf("\nvpow : \n");  
  vpow(in, out[0], out2);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out2[i]);

    }


  out[0] = 1;
  out[1] = 2;
  out[2] = 4.85956;
  out[3] = 4;

  printf("\nvpow vect : \n");  
  vpow_vect(in, out, out2);

  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf\n", out2[i]);

    }

  printf("\nvsincos : \n");  
  vsincos(in, out, out2);
  for (i = 0; i < PATH_LENGTH; i++)
    {
      printf("%lf | %lf\n", out[i], out2[i]);
      printf("%lf \n", out[i] * out[i] + out2[i] * out2[i]);
    }

  return 0;
}
