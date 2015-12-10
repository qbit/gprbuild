#pragma once
#include <stdlib.h>

struct _internal_meminfo_t
{
	unsigned long long mem_size;
	unsigned long long magic;
	
}__attribute__ ((aligned (16)));

void *malloc(size_t size);

void *realloc(void *ptr, size_t size);

void *calloc(size_t nmemb, size_t size);

void free(void *ptr);

void *mmap (void *start, size_t len, int prot, int flags, int fd, off_t offset);
// #define malloc(size) xmalloc(size)

// #define free(ptr) xfree(ptr)

// #define calloc(nmemb, size)		xcalloc(nmemb, size)

// #define realloc(ptr, size)		xrealloc(ptr, size)




