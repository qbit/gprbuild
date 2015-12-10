#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include <pthread.h>
#include "gmalloc.h"
#include "internal_alloc.h"


#ifdef malloc
#undef malloc
#endif

#ifdef realloc
#undef realloc
#endif

#ifdef calloc
#undef calloc
#endif

#ifdef free
#undef free
#endif

#ifdef mmap
#undef mmap
#endif

#ifdef mremap
#undef mremap
#endif

#ifdef munmap
#undef munamp
#endif


void *malloc(size_t size)
{
	void *ptr = NULL;

	ptr = _internal_malloc(size);;
	return ptr; 
}

void *realloc(void *ptr, size_t size)
{
	void *ret = NULL;

	ret = _internal_realloc(ptr, size);;
	return ret;
}

void *calloc(size_t nmemb, size_t size)
{
	void *ret = NULL;
	ret = _internal_calloc(nmemb, size);;
	return ret; 
}

void free(void *ptr)
{
	_internal_free(ptr);
}

void *mmap (void *start, size_t len, int prot, int flags, int fd, off_t offset)
{
	void *ret = NULL;
	ret = _internal_mmap(start, len, prot, flags, fd, offset);
	return ret;
}

int munmap (void *start, size_t len)
{
	int ret = 0;

	ret = _internal_munmap (start, len);

	return ret; 
}


