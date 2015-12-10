#ifndef SHAREDMEM_H
#define SHAREDMEM_H

   #include <constants.h>

   typedef struct
   {
      int    mnID;
      char * mpLoc;
      teBool meCreated;
   } tsSharedMem;

   void sharedMemInit( tsSharedMem * apSharedMem );

   int sharedMemCreate( tsSharedMem * apSharedMem, char * apKey, int anSize );

   int sharedMemRemove( char * apKey );

   int sharedMemFree( tsSharedMem * apSharedMem );

   teBool sharedMemCreated( tsSharedMem * apSharedMem );

#endif
