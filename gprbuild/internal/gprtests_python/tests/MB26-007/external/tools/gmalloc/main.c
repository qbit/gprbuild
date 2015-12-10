#include <stdio.h>
#include <stdlib.h>

#define fill_mem(mem, size)						\
	{											\
												\
		unsigned j = 0;							\
		for (j = 0; j < size; j++)				\
		{										\
			mem[j] = j % 10;					\
		}										\
	}




int main()
{
	char *ptr[15];
	
	ptr[0] = malloc(15);
	fill_mem(ptr[0], 15);	

	ptr[1] = malloc(15);
	fill_mem(ptr[1], 15);

	ptr[2] = malloc(15);
	fill_mem(ptr[2], 15);

	ptr[3] = malloc(15);
	fill_mem(ptr[3], 15);
	
	free(ptr[3]);
	free(ptr[2]);

	ptr[0] = malloc(15);

	ptr[0] = malloc(15);
	ptr[0] = malloc(15);

	ptr[4] = calloc(2, 32);
	ptr[5] = calloc(10, 15);
	ptr[6] = calloc(10, 15);
	ptr[7] = calloc(10, 15);

	ptr[4][0] = 10;

	free(ptr[4]);
	free(ptr[5]);
	free(ptr[6]);
	free(ptr[7]);


	ptr[8] = malloc(1960);

	ptr[8] = realloc(ptr[8], 1900);
	ptr[8] = realloc(ptr[8], 2048);


	ptr[8] = realloc(ptr[8], 0);

	return 0;
}
