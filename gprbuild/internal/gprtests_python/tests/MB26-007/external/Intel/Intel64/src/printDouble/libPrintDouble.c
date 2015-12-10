#include <stdio.h>
#include <string.h>

union transDouble
{
	double value;
	unsigned char bits[sizeof(double)];
};


void	PrintDouble(char *str, double val_in, double value1, double value2)
{
	union transDouble tools;
	unsigned i = 0;
	unsigned k = 0;
	unsigned int bit = 0;
	char input[sizeof(double) * 8];
	char output1[sizeof(double) * 8];
	char output2[sizeof(double) * 8];



	tools.value = val_in;

	memset((void *)input, 0, sizeof(double));
	for (k = 0, i = 0; i < sizeof(double); i++, k += 8)
	{
		input[k]     = (tools.bits[i] & 0x01);
		input[k + 1] = (tools.bits[i] & 0x02) >> 1;
		input[k + 2] = (tools.bits[i] & 0x04) >> 2;
		input[k + 3] = (tools.bits[i] & 0x08) >> 3;
		input[k + 4] = (tools.bits[i] & 0x10) >> 4;
		input[k + 5] = (tools.bits[i] & 0x20) >> 5;
		input[k + 6] = (tools.bits[i] & 0x40) >> 6;
		input[k + 7] = (tools.bits[i] & 0x80) >> 7;

	}

	/* ///////////////////////////////////////////////////////////////////////////// */



	tools.value = value1;

	memset((void *)output1, 0, sizeof(double));
	for (k = 0, i = 0; i < sizeof(double); i++, k += 8)
	{
		output1[k]     = (tools.bits[i] & 0x01);
		output1[k + 1] = (tools.bits[i] & 0x02) >> 1;
		output1[k + 2] = (tools.bits[i] & 0x04) >> 2;
		output1[k + 3] = (tools.bits[i] & 0x08) >> 3;
		output1[k + 4] = (tools.bits[i] & 0x10) >> 4;
		output1[k + 5] = (tools.bits[i] & 0x20) >> 5;
		output1[k + 6] = (tools.bits[i] & 0x40) >> 6;
		output1[k + 7] = (tools.bits[i] & 0x80) >> 7;

	}

	/* ///////////////////////////////////////////////////////////////////////////// */
	tools.value = value2;

	memset((void *)output2, 0, sizeof(double));
	for (k = 0, i = 0; i < sizeof(double); i++, k += 8)
	{
		output2[k]     = (tools.bits[i] & 0x01);
		output2[k + 1] = (tools.bits[i] & 0x02) >> 1;
		output2[k + 2] = (tools.bits[i] & 0x04) >> 2;
		output2[k + 3] = (tools.bits[i] & 0x08) >> 3;
		output2[k + 4] = (tools.bits[i] & 0x10) >> 4;
		output2[k + 5] = (tools.bits[i] & 0x20) >> 5;
		output2[k + 6] = (tools.bits[i] & 0x40) >> 6;
		output2[k + 7] = (tools.bits[i] & 0x80) >> 7;

	}

	/*////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////*/
	for (i = 0; i < sizeof(double) * 8; i++)
	{
		if (output1[i] != output2[i])
		{
			printf("\nINTEL : (%s), Difference Value for %lf\n", str, val_in);
			break;
		}
	}

	if (i >= sizeof(double) * 8)
		return ;
		
	printf("INTEL : %lf : (Input) \nINTEL : ", val_in);
	for (i = 0; i < sizeof(double) * 8; i++)
	{
		bit = input[sizeof(double) * 8 - 1 - i];

		printf ("%d ",  bit);

		if ( i == 0 )
			printf(" | ");
		else if (i == 11 )
			printf(" | ");
		else if (i != 0 && (i % 4) == 0)
			printf("  ");


	}
	printf("\n");
	/*///////////////////////////////////////////////////////////////////////////*/
	printf("INTEL : %lf : (Output 1)\nINTEL : ", value1);
	for (i = 0; i < sizeof(double) * 8; i++)
	{
		bit = output1[sizeof(double) * 8 - 1 - i];

		printf ("%d ",  bit);

		if ( i == 0 )
			printf(" | ");
		else if (i == 11 )
			printf(" | ");
		else if (i != 0 && (i % 4) == 0)
			printf("  ");


	}
	printf("\n");


	printf("INTEL : %lf : (Output 2)\nINTEL : ", value2);
	for (i = 0; i < sizeof(double) * 8; i++)
	{
		bit = output2[sizeof(double) * 8 - 1 - i];

		printf ("%d ",  bit);

		if ( i == 0 )
			printf(" | ");
		else if (i == 11 )
			printf(" | ");
		else if (i != 0 && (i % 4) == 0)
			printf("  ");


	}
	printf("\nINTEL : ---------------------------------------------------------------------------------------------------------------------------------------------------------------------\n");


}
