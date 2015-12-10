#include<stdio.h>
#include "valgrind.h"

int main(int argc, char **argv){
   int i,j = 0;
   int t = i + j;
   valgrind_start();
      printf("hello world!");
   valgrind_stop();
}
