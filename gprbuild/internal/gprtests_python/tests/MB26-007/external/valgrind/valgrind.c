#include <valgrind/callgrind.h>


void valgrind_start(){
   CALLGRIND_START_INSTRUMENTATION;
}

void valgrind_stop(){
   CALLGRIND_STOP_INSTRUMENTATION;
}

void valgrind_dump_stats(){
   CALLGRIND_DUMP_STATS;
}

void valgrind_zero_stats(){
   CALLGRIND_ZERO_STATS;
}
