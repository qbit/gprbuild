#include <stdio.h>
#include "PrintDouble.h"



int main(int argc, char **argv)
{
	double input_Double = 0.0;
	double input = 1.01f;

	double output1 = 1.01f;

	double output2 = 1.010001f;

	/* long double lf = 0.0f; */
	/* long long double ld = 0.0f; */



	PrintDouble(&input, &output1, &output2);

	/* if (argc > 1) */
	/* 	sprintf(&input_Ldouble, "%lf", argv[1]); */
	return 0;
}
