#ifndef _SVML_INTERFACE_
#define _SVML_INTERFACE_

// The defined length
#define PATH_LENGTH 4

void	vexp( double * __restrict__ in, double * __restrict__ out );

void	vlog( double * __restrict__ in, double * __restrict__ out );

void	vsin( double * __restrict__ in, double * __restrict__ out );

void	vcos( double * __restrict__ in, double * __restrict__ out );

void	vsqrt( double * __restrict__ in, double * __restrict__ out );

void	vpow( double * __restrict__ in, double pow, double * __restrict__ out);

void	vpow_vect( double * __restrict__ in, double * __restrict__ p, double * __restrict__ out);

void	vfloor( double * __restrict__ in, double * __restrict__ out );

void	vsincos( double * __restrict__ in, double * __restrict__ out_sin, double * __restrict__ out_cos );


#endif /* _SVML_INTERFACE_ */
