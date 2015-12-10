#include<sched.h>
#include<unistd.h>
#include<stdio.h>

//cpu_id is between 0 and sysconf( _SC_NPROCESSORS_ONLN ) - 1 
int set_cpu( int cpu_id ){
   cpu_set_t cpu_mask;
   int status;

   //no cpus allowed 
   CPU_ZERO( &cpu_mask ); 
   //except the one we've asked for
   CPU_SET( cpu_id, &cpu_mask );
   status = sched_setaffinity( getpid(), sizeof( cpu_set_t ), &cpu_mask );

   return status;
}


