#ifndef SEMAPHORE_H
#define SEMAPHORE_H
   #include <sys/types.h>
   #include <sys/ipc.h>
   #include <sys/sem.h>
   #include <constants.h>

   typedef struct
   {
      int    mnID;
      char * mpLoc;
      teBool meCreated;
   } tsSema;

   union semun {
      int             val;
      struct semid_ds *buf;
      ushort_t        *array;
   } arg ;

   int semaInit( tsSema * apSema );

   int semaCreate( tsSema * apSema, char * apKey );

   int semaValue( tsSema * apSema );

   int semaFree( tsSema * apSema );

   int semaLock( tsSema * apSema );

   int semaUnlock( tsSema * apSema );

   teBool semaCreated( tsSema * apSema );

#endif
