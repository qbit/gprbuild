#ifndef MEMMAP_H
#define MEMMAP_H

   #include <constants.h>

   typedef enum { eeSmall = 0, eeMedium = 1, eeLarge = 2, eeSizes = 3 } teSize;

   typedef enum { eePORT = 0, eeSTBD = 1 } teProcessor;

   void * memMap( unsigned int anAddress, teSize aeSize, teProcessor aeProcessor );

   int memInit( teBool aeClear );

   void * memRevMap( unsigned int anAddress );

   int memRemove( void );

   void memSetPath( char * apPath );

   char * memGetPath( void );

#endif
