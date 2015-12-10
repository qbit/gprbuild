#include <pthread.h>

extern void ada_routine (void);

void *thread_body (void *param)
{
  ada_routine ();
  return NULL;
}

void c_func (void)
{
  pthread_t t;

  pthread_create(&t, NULL, thread_body, NULL);
  pthread_join (t, NULL);
}
