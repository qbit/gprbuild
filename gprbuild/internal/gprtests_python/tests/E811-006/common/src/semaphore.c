#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <semaphore.h>

int semaInit( tsSema * apSema )
{
   apSema->mpLoc = NULL;

   apSema->mnID = gnError;

   apSema->meCreated = eeFalse;
}


int semaCreate( tsSema * apSema, char * apKey )
{
   key_t lnKey;

   int lnFlags = IPC_CREAT | IPC_EXCL | SEM_A | SEM_R;

   union semun luSet;

   lnKey = ftok( apKey, 'W' );

   if ( lnKey == gnError )
   {
      fprintf( stderr, "ftok error: %s for %s\n", strerror( errno ), apKey );

      return gnError;
   }

   apSema->mnID = semget( lnKey, 1, lnFlags );

   if ( apSema->mnID == gnError )
   {
      if ( errno == EEXIST )
      {
         lnFlags = IPC_CREAT | SEM_A | SEM_R;

         apSema->mnID = semget( lnKey, 1, lnFlags );

         if ( apSema->mnID == gnError )
         {
            fprintf( stderr, "semget error 2: %s\n", strerror( errno ) );

            return gnError;
         }
      }
      else
      {
         fprintf( stderr, "semget error 1: %s\n", strerror( errno ) );

         return gnError;
      }
   }
   else
   {
      apSema->meCreated = eeTrue;

      luSet.val = 1;

      if ( semctl( apSema->mnID, 0, SETVAL, luSet ) == gnError )
      {
         fprintf( stderr, "semctl error 1: %s\n", strerror( errno ) );

         return gnError;
      }
   }

   return 0;
}


int semaValue( tsSema * apSema )
{
   union semun luGet;

   int lnValue;

   if ( apSema->mnID != gnError )
   {
      lnValue = semctl( apSema->mnID, 0, GETVAL, luGet );

      if ( lnValue == gnError )
      {
         fprintf( stderr, "semctl error 3: %s\n", strerror( errno ) );

         return gnError;
      }

      return lnValue;
   }
   else
   {
      fprintf( stderr, "semaFree error: invalid semaphore\n" );

      return gnError;
   }
}

int semaFree( tsSema * apSema )
{
   if ( apSema->mnID != gnError )
   {
      if ( semctl( apSema->mnID, 0, IPC_RMID, 0 ) == gnError )
      {
         fprintf( stderr, "semctl error 2: %s\n", strerror( errno ) );

         return gnError;
      }

      apSema->mnID = gnError;

      return 0;
   }
   else
   {
      fprintf( stderr, "semaFree error: invalid semaphore\n" );

      return gnError;
   }
}


int semaLock( tsSema * apSema )
{
   const struct sembuf lsSemOps[] =
   {
      {
         0, /* sem_num */
        -1, /* sem_op  */
         0, /* sem_flg */
      }
   };

   if ( apSema->mnID != gnError )
   {
      if ( semop( apSema->mnID, ( struct sembuf * ) lsSemOps, 1 ) == gnError )
      {
         fprintf( stderr, "semop error: %s\n", strerror( errno ) );

         return gnError;
      }

      return 0;
   }
   else
   {
      fprintf( stderr, "semaLock error: invalid semaphore\n" );

      return gnError;
   }
}


int semaUnlock( tsSema * apSema )
{
   const struct sembuf lsSemOps[] =
   {
      {
         0, /* sem_num */
         1, /* sem_op  */
         0, /* sem_flg */
      }
   };

   if ( apSema->mnID != gnError )
   {
      if ( semop( apSema->mnID, ( struct sembuf * ) lsSemOps, 1 ) == gnError )
      {
         fprintf( stderr, "semop error: %s\n", strerror( errno ) );

         return gnError;
      }

      return 0;
   }
   else
   {
      fprintf( stderr, "semaUnlock error: invalid semaphore\n" );

      return gnError;
   }
}


teBool semaCreated( tsSema * apSema )
{
   return apSema->meCreated;
}
