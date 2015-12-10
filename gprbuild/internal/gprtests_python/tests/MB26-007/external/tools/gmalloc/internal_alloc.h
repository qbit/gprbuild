#pragma once

extern pthread_mutex_t *mutex;

void _internal_free(void *ptr);

void *_internal_malloc(size_t size);

void *_internal_realloc(void *ptr, size_t size);

void *_internal_calloc(size_t nmemb, size_t size);

void *_internal_mmap (void *start, size_t len, int prot, int flags, int fd, off_t offset);

int _internal_munmap (void *start, size_t len);

void * mremap (void *start, size_t old_len, size_t len, int flags,  ...);


