#include <stdlib.h>
#include <stdio.h>
#define __USE_GNU
#include <dlfcn.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/mman.h>
#include <pthread.h>
#include <stdio.h>
#include "gmalloc.h"

static size_t mem_size_max;
static size_t mem_size;
void    *handle;

#define MAGIC 0xfeedbeaf

typedef unsigned long long off64_t;

typedef void *(*func_t)();
typedef void (*func_free_t)(void *ptr);


static void *(*malloc_ptr) (size_t);
static void *(*realloc_ptr) (void *, size_t);
static void *(*calloc_ptr) (size_t, size_t);
static void (*free_ptr) (void *);

static void *(*mmap_ptr) (void *, size_t, int, int, int, off_t);
static void *(*mmap64_ptr) (void *, size_t, int, int, int, off64_t);
static int (*munmap_ptr) (void *, size_t);
static void *(*mremap_ptr) (void *, size_t, size_t, int, void *);

static int initialized = -1;
static int not_me = 0;

static pthread_mutex_t *mutex;

#define INIT_INFO()								\
	if (initialized == -1)						\
	{											\
		initialize_gmalloc();					\
		initialized = 1;						\
	}

static void
__attribute__((constructor))
initialize_gmalloc() 
{	

	/* find the address of function and data objects */
	//start_sp = GETSP ();
	if (initialized == -1)
	{
		malloc_ptr = (void *(*) (size_t)) dlsym (RTLD_NEXT, "malloc");
		mutex = (pthread_mutex_t *)(*malloc_ptr)(sizeof(pthread_mutex_t));
		pthread_mutex_init(mutex, NULL);
		realloc_ptr = (void *(*) (void *, size_t)) dlsym (RTLD_NEXT, "realloc");
		calloc_ptr = (void *(*) (size_t, size_t)) dlsym (RTLD_NEXT, "calloc");
		free_ptr = (void (*) (void *)) dlsym (RTLD_NEXT, "free");
		mmap_ptr = (void *(*) (void *, size_t, int, int, int, off_t)) dlsym (RTLD_NEXT,
																			 "mmap");
		mmap64_ptr =
			(void *(*) (void *, size_t, int, int, int, off64_t)) dlsym (RTLD_NEXT,
																		"mmap64");
		mremap_ptr = (void *(*) (void *, size_t, size_t, int, void *)) dlsym (RTLD_NEXT,
																			  "mremap");
		munmap_ptr = (int (*) (void *, size_t)) dlsym (RTLD_NEXT, "munmap");	
		initialized = 1;
		//printf("Initialize shared library\n");
		/* printf("Initialize shared library free    %p\n", free_ptr); */
		/* printf("Initialize shared library malloc  %p\n", malloc_ptr); */
		/* printf("Initialize shared library realloc %p\n", realloc_ptr); */
		sleep(1);

		/* mem_size_max	= (size_t *)malloc_ptr(sizeof(size_t)); */
		/* mem_size		= (size_t *)malloc_ptr(sizeof(size_t)); */

		/* *mem_size_max	= 0; */
		initialized = 1;
	}
	not_me = 0;

}

__attribute__((destructor))
static void destroy_gmalloc() 
{
	const char *env = getenv ("GPRIME_NOPEAK");
	/* pthread_mutex_destroy(mutex); */
	/* (*free_ptr)(mutex); */
	if (env == NULL)
		fprintf(stdout, "MEM> Heap peak %ld bytes\n", mem_size_max);

}


void _internal_free(void *ptr)
{
	struct _internal_meminfo_t *head = NULL;


	////fprintf(stderr, "call function : %s with virtual adress %p\n", __FUNCTION__, ptr);
	/* Determine real implementation if not already happened.  */
	if (__builtin_expect (initialized <= 0, 0))
    {
		if (initialized == -1)
		{
			return;
		}
		initialize_gmalloc ();
    }
	//////fprintf(stderr, "call function : %s pass init\n", __FUNCTION__);
    /* If this is not the correct program just use the normal function.  */
	pthread_mutex_lock(mutex);
	////fprintf(stderr, "LOCK : %s\n", __FUNCTION__);

	if (ptr == NULL)
	{
		////fprintf(stderr, "UNLOCK : %s\n", __FUNCTION__);
		pthread_mutex_unlock(mutex);
		return;
	}

	head = (struct _internal_meminfo_t *)(((char *)ptr) - sizeof(struct _internal_meminfo_t));
	////fprintf(stderr, "%p Get Head %p\n", ptr, (void *)head);
	//////fprintf(stderr, "call function : %s pass Head\n", __FUNCTION__);
	//*mem_size_max = *mem_size_max - head->mem_size;
	if (head->magic != MAGIC)
	{
		(*free_ptr)((void *)ptr);
		pthread_mutex_unlock(mutex);
		return;
	}

	mem_size = mem_size - head->mem_size;
	//fprintf(stderr, "Call  Free : %p(%lld)\n", (void *)head, head->mem_size);
	//////fprintf(stderr, "call function : %s Call the free function\n", __FUNCTION__);
	(*free_ptr)((void *)head);
	////fprintf(stderr, "call function : %s Call the free function\n", __FUNCTION__);

	pthread_mutex_unlock(mutex);
	////fprintf(stderr, "LOCK : %s\n", __FUNCTION__);

}


void *_internal_malloc(size_t size)
{
	char *ptr = NULL;
	struct _internal_meminfo_t *pinf;

	///intf(stderr, "call function : %s ", __FUNCTION__);
	/* Determine real implementation if not already happened.  */
	if (__builtin_expect (initialized <= 0, 0))
    {
		if (initialized == -1)
			return NULL;
		initialize_gmalloc ();
    }
	pthread_mutex_lock(mutex);
	////fprintf(stderr, "LOCK : %s\n", __FUNCTION__);

	pinf = (struct _internal_meminfo_t *)(*malloc_ptr)(sizeof(struct _internal_meminfo_t) +  size);
	pinf->mem_size = size;
	pinf->magic = MAGIC;
	ptr = (char *)pinf;

	mem_size = mem_size + size;
	if (mem_size > mem_size_max)
		mem_size_max = mem_size;

	////////fprintf(stderr, "Call  Malloc :  %p(%ld)\n", ptr, size);
	//fprintf(stderr, "call function : %s with adress %p real adress : %p(%ld)\n", __FUNCTION__, (void *)(ptr + sizeof(struct _internal_meminfo_t)), ptr, size); 
	////fprintf(stderr, "UNLOCK : %s\n", __FUNCTION__);
	pthread_mutex_unlock(mutex);
	return (void *)(ptr + sizeof(struct _internal_meminfo_t));
}

void *_internal_realloc(void *ptr, size_t size)
{
	char * ptr_c = (char *)ptr;
	struct _internal_meminfo_t *pinf;
	

	/* Determine real implementation if not already happened.  */
	if (__builtin_expect (initialized <= 0, 0))
    {
		if (initialized == -1)
			return NULL;
		initialize_gmalloc ();
    }


	if (size == 0 && ptr == NULL)
	{
		return NULL;
	}
	if (size == 0)
	{
		_internal_free(ptr);
	}
	else if (ptr == NULL)
	{
		ptr_c = (char *)_internal_malloc(size);
		return (void *)(ptr_c);
	}
	else
	{
		pthread_mutex_lock(mutex); 
		//fprintf(stderr, "LOCK : %s\n", __FUNCTION__);
		pinf = (struct _internal_meminfo_t *)(ptr_c - sizeof(struct _internal_meminfo_t));

		/* if (size <= pinf->mem_size) */
		/* 	return ptr_c; */

		

		if (pinf->magic != MAGIC)
			return (char *)(*realloc_ptr)((void *)ptr_c, sizeof(struct _internal_meminfo_t) + size);
		else
		{
			mem_size = mem_size - pinf->mem_size; 
			//fprintf(stderr, "Call  Realloc : %p(%lld --> %ld %ld)\n", ptr_c, pinf->mem_size, size, sizeof(struct _internal_meminfo_t) + size);
			ptr_c = (char *)(*realloc_ptr)((void *)pinf, sizeof(struct _internal_meminfo_t) + size);
		}

		pinf = (struct _internal_meminfo_t *)(ptr_c);
		pinf->mem_size = size;
		pinf->magic = MAGIC;

		mem_size = mem_size + size;
		if (mem_size > mem_size_max)
			mem_size_max = mem_size;

		//fprintf(stderr, "UNLOCK : %s\n", __FUNCTION__);
		pthread_mutex_unlock(mutex);
		

		return (void *)(ptr_c + sizeof(struct _internal_meminfo_t));
	}
	return NULL;
}

void *_internal_calloc(size_t nmemb, size_t size)
{
	char * ptr;
	size_t nbBlock = (sizeof(struct _internal_meminfo_t) / size) + 1;
	struct _internal_meminfo_t *pinf;

	////fprintf(stderr, "call function : %s\n", __FUNCTION__);
	/* Determine real implementation if not already happened.  */
	if (__builtin_expect (initialized <= 0, 0))
    {
		if (initialized == -1)
			return NULL;
		initialize_gmalloc ();
    }

	/* If this is not the correct program just use the normal function.  */
	pthread_mutex_lock(mutex);
	////fprintf(stderr, "LOCK : %s\n", __FUNCTION__);

	//fprintf(stderr, "INFO DD Calloc nmemb %ld, size %ld\n", nmemb, size);
	
	ptr = (char *)(*calloc_ptr)(nmemb + nbBlock, size);
	////fprintf(stderr, "INFO DD Calloc sizeof struct %ld nbblock add %ld ptr %p ", sizeof(struct _internal_meminfo_t), nbBlock, (void *)calloc_ptr);
	pinf = (struct _internal_meminfo_t *)ptr;

	pinf->mem_size = nmemb * size;
	pinf->magic = MAGIC;

	mem_size = mem_size + (nmemb * size);

	if (mem_size > mem_size_max)
		mem_size_max = mem_size;
	////////fprintf(stderr, "Call  Calloc : %p(%ld)\n", ptr, size);
	ptr = ptr + sizeof(struct _internal_meminfo_t);
	////fprintf(stderr, "CALLOC OK \n");
	////fprintf(stderr, "UNLOCK : %s\n", __FUNCTION__);
	pthread_mutex_unlock(mutex);

	return (void *)ptr;
}

void *_internal_mmap (void *start, size_t len, int prot, int flags, int fd, off_t offset)
{
  void *result = NULL;

  ////fprintf(stderr, "call function : %s alloc : ", __FUNCTION__);
  /* Determine real implementation if not already happened.  */
  if (__builtin_expect (initialized <= 0, 0))
    {
      if (initialized == -1)
		  return NULL;
      initialize_gmalloc ();
    }

  /* Always get a block.  We don't need extra memory.  */
  /* mem_size = mem_size + (len); */

  /* if (mem_size > mem_size_max) */
  /* 	  mem_size_max = mem_size; */

  result = (*mmap_ptr) (start, len, prot, flags, fd, offset);
  ////fprintf(stderr, "%p\n", (void *)result);
 
 /* Return the pointer to the user buffer.  */
  return result;
}


/* `mmap' replacement.  We do not have to keep track of the sizesince
   `munmap' will get it as a parameter.  */
void * _internal_mremap (void *start, size_t old_len, size_t len, int flags,  ...)
{
	void *result = NULL;
	va_list ap;

	va_start (ap, flags);
	void *newaddr = (flags & MREMAP_FIXED) ? va_arg (ap, void *) : NULL;
	va_end (ap);

	/* Determine real implementation if not already happened.  */
	if (__builtin_expect (initialized <= 0, 0))
    {
		if (initialized == -1)
			return NULL;
	   initialize_gmalloc ();
    }

	/* Always get a block.  We don't need extra memory.  */
	result = (*mremap_ptr) (start, old_len, len, flags, newaddr);

	/* Return the pointer to the user buffer.  */
	return result;
}


/* `munmap' replacement.  */
int _internal_munmap (void *start, size_t len)
{
  int result;

  /* Determine real implementation if not already happened.  */
  if (__builtin_expect (initialized <= 0, 0))
    {
      if (initialized == -1)
	return -1;
      initialize_gmalloc ();
    }
  /* mem_size = mem_size - len; */
  /* Do the real work.  */
  result = (*munmap_ptr) (start, len);

  return result;
}
