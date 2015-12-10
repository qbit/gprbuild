#include <mathimf.h>
#include <stdio.h>

#include "svml_interface.h"

/* ------------------ */
/* Exp vector	      */
/* ------------------ */
void	vexp( double * __restrict__ in, double * __restrict__ out)
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
      out[ i ] = exp( in[ i ] );
    }
}


/* ------------------ */
/* Log vector	      */
/* ------------------ */

void	vlog( double * __restrict__ in, double * __restrict__ out )
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
      out[ i ] = log( in[ i ] );
    }
}

/* ------------------ */
/* Sin vector	      */
/* ------------------ */

void	vsin( double * __restrict__ in, double * __restrict__ out )
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
      out[ i ] = sin( in[ i ] );
    }
}

/* ------------------ */
/* Cos vector	      */
/* ------------------ */

void	vcos( double * __restrict__ in, double * __restrict__ out )
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
      out[ i ] = cos( in[ i ] );
    }
}

/* ------------------ */
/* Sqrt vector	      */
/* ------------------ */

void	vsqrt( double * __restrict__ in, double * __restrict__ out )
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
      out[ i ] = sqrt( in[ i ] );
    }
}

/* ------------------ */
/* floor vector	      */
/* ------------------ */

void	vfloor( double * __restrict__ in, double * __restrict__ out )
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
      out[ i ] = floor( in[ i ] );
    }
}

/* ------------------ */
/* Pow vector	      */
/* ------------------ */

void	vpow( double * __restrict__ in, double p, double * __restrict__ out)
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
		out[ i ] = pow( in[ i ], p);
    }
}

/* ------------------ */
/* Pow vector by vect */
/* ------------------ */

void	vpow_vect( double * __restrict__ in, double * __restrict__ p, double * __restrict__ out)
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
		out[ i ] = pow( in[ i ], p [ i ]);
    }
}


/* ------------------ */
/* Sincos vector      */
/* ------------------ */

void	vsincos( double * __restrict__ in, double * __restrict__ out_sin, double * __restrict__ out_cos )
{
  unsigned i;

  for ( i = 0; i < PATH_LENGTH; i++ )
    {
      sincos( in[ i ] , &(out_sin[i]), &(out_cos[i]));
    }
}

