#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>
#include <string.h>

#include <sharedMem.h>

void sharedMemInit( tsSharedMem * apSharedMem )
{
   apSharedMem->mpLoc = NULL;

   apSharedMem->mnID = gnError;

   apSharedMem->meCreated = eeFalse;
};

int sharedMemCreate( tsSharedMem * apSharedMem, char * apKey, int anSize )
{
   key_t lnKey;

   int lnFlags = IPC_CREAT | IPC_EXCL | SHM_R | SHM_W;

   lnKey = ftok( apKey, 'W' );

   if ( lnKey == gnError )
   {
      fprintf( stderr, "ftok error: %s for %s\n", strerror( errno ), apKey );

      return gnError;
   }

   apSharedMem->mnID = shmget( lnKey, anSize, lnFlags );

   if ( apSharedMem->mnID == gnError )
   {
      if ( errno == EEXIST )
      {
         lnFlags = SHM_R | SHM_W;

         apSharedMem->mnID = shmget( lnKey, anSize, lnFlags );

         if ( apSharedMem->mnID == gnError )
         {
            fprintf( stderr, "shmget error: %s\n", strerror( errno ) );

            return gnError;
         }
      }
      else
      {
         fprintf( stderr, "shmget error: %s\n", strerror( errno ) );

         return gnError;
      }
   }
   else
   {
      apSharedMem->meCreated = eeTrue;
   }

   apSharedMem->mpLoc = shmat( apSharedMem->mnID, 0, 0 );

   if ( ( ( int ) apSharedMem->mpLoc ) == gnError )
   {
      fprintf( stderr, "shmat error: %s\n", strerror( errno ) );

      return gnError;
   }

   return 0;
}


int sharedMemRemove( char * apKey )
{
   key_t lnKey;

   int   mnID;

   int lnFlags = IPC_EXCL | SHM_R | SHM_W;


   lnKey = ftok( apKey, 'W' );

   if ( lnKey == gnError )
   {
      fprintf( stderr, "ftok error: %s for %s\n", strerror( errno ), apKey );

      return gnError;
   }

   mnID = shmget( lnKey, 0, lnFlags );

   if ( mnID == gnError )
   {
      if ( errno == ENOENT )
      {
         return 0;
      }
      else
      {
         fprintf( stderr, "shmget error: %s\n", strerror( errno ) );

         return gnError;
      }
   }
   else
   {
      if ( shmctl( mnID, IPC_RMID, 0 ) == gnError )
      {
         fprintf( stderr, "shmctl error: %s\n", strerror( errno ) );

         return gnError;
      }
   }

   return 0;
};


int sharedMemFree( tsSharedMem * apSharedMem )
{
   if ( shmctl( apSharedMem->mnID, IPC_RMID, 0 ) == gnError )
   {
      if ( errno != EINVAL )
      {
         fprintf( stderr, "shmctl error: %s\n", strerror( errno ) );

         return gnError;
      }
   }

   return 0;
};


teBool sharedMemCreated( tsSharedMem * apSharedMem )
{
   return apSharedMem->meCreated;
};
