#include<sched.h>
#include<unistd.h>
#include<stdio.h>

#define BITS_PER_BYTE 8
void display_cpu_set( cpu_set_t cpu_mask )
{
   int i;
   
   for(i=0; i< sizeof( cpu_set_t ) * BITS_PER_BYTE; i++)
   {
      if (CPU_ISSET( i, &cpu_mask ))
      {
         printf("%d : ON\n", i);
      }
      else
      {
         printf("%d : OFF\n", i);
      }
   }
}

//cpu_id is between 0 and sysconf( _SC_NPROCESSORS_ONLN ) - 1 
int set_cpu( int cpu_id ){
   cpu_set_t cpu_mask;
   int status;

   //no cpus allowed 
   CPU_ZERO( &cpu_mask ); 
   //except the one we've asked for
   CPU_SET( cpu_id, &cpu_mask );
   status = sched_setaffinity( getpid(), sizeof( cpu_set_t ), &cpu_mask );

   //let's check this 
   sched_getaffinity( getpid(), sizeof( cpu_set_t ), &cpu_mask );
   display_cpu_set( cpu_mask );
   return status;
}


int main( int argc, char **argv )
{
   int status = set_cpu( 7 );
   printf("Status : %d\n", status );
   return 0;
}
