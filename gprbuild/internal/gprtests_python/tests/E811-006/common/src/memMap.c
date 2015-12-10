#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <constants.h>
#include <memMap.h>
#include <dirent.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sharedMem.h>

#define gnMaxSegments 64

static int gnSegments = 0;

static char gaSizeID[] = "sml";

static char * gpPath = "../u_dat";

#define gnSmallSegmentSize 0x1000
#define gnMedSegmentSize 0x20000
#define gnLargeSegmentSize 0x100000

static int gnSegmentSize[] =
  { gnSmallSegmentSize, gnMedSegmentSize, gnLargeSegmentSize };

#define lowMask( anSegmentSize ) ( anSegmentSize - 1 )

#define highMask( anSegmentSize ) ( 0xFFFFFFFF - lowMask( anSegmentSize ) )

typedef struct
{
   void *      mnBase;
   void *      mnMap;
   int         mnSize;
   teProcessor meProcessor;
   tsSharedMem msSharedMem;
   teBool      meUsed;
} tsMemInfo;

static tsMemInfo gsMemInfo[ gnMaxSegments ] = { 0 };

int matchName( char * apMatch, char * apPoss )
{
   int    lnIndex;
   int    lnIndex2;
   char * lpWild = "psabcdef0123456789";
   teBool leMatched = eeFalse;

   if (  strlen( apMatch ) != strlen( apPoss ) )
   {
      return eeFalse;
   }

   for ( lnIndex = 0; lnIndex < strlen( apMatch ); lnIndex++ )
   {
      if ( apMatch[ lnIndex ] == '#' )
      {
         leMatched = eeFalse;

         for ( lnIndex2 = 0; lnIndex2 < strlen( lpWild ); lnIndex2++ )
         {
            if ( apPoss[ lnIndex ] == lpWild[ lnIndex2 ] )
            {
               leMatched = eeTrue;

               break;
            }
         }

         if ( ! leMatched )
         {
            return eeFalse;
         }
      }
      else if ( apMatch[ lnIndex ] == '@' )
      {
         leMatched = eeFalse;

         for ( lnIndex2 = 0; lnIndex2 <= eeSizes; lnIndex2++ )
         {
            if ( apPoss[ lnIndex ] == gaSizeID[ lnIndex2 ] )
            {
               leMatched = eeTrue;

               break;
            }
         }

         if ( ! leMatched )
         {
            return eeFalse;
         }

      }
      else if ( apMatch[ lnIndex ] != apPoss[ lnIndex ] )
      {
         return eeFalse;
      }
   }

   return eeTrue;
}


int memInit( teBool aeClear )
{
   struct dirent * lpEntry;
   DIR *           lpDir;
   int             lnFD;
   char *          lpFileName  = "mem#########@.dat";
   int             lnFileFlags = O_RDONLY;
   int             lnStatus;
   int             lnFileSize;
   int             lnIndex;

   if ( chdir( gpPath ) == 0 )
   {
      lpDir = opendir( gpPath );

      if ( aeClear )
      {
         printf( "Initializing memory...\n" );
      }

      while ( ( lpEntry = readdir( lpDir ) ) != NULL )
      {
         if ( matchName( lpFileName, lpEntry->d_name ) )
         {
            char     lnProcessor;
            teProcessor leProcessor;
            char     lnSize;
            teSize   leSize;
            char *   lpAddr;

            sscanf( lpEntry->d_name, "mem%c%08x%c.dat", & lnProcessor, & lpAddr,
              & lnSize );

            if ( lnSize == 's' )
            {
               leSize = eeSmall;
            }
            else if ( lnSize == 'm' )
            {
               leSize = eeMedium;
            }
            else
            {
               leSize = eeLarge;
            }

	    if ( lnProcessor == 's' )
            {
               leProcessor = eeSTBD;
               lnFileSize = gnSegmentSize[ leSize ];
            }
            else
            {
               leProcessor = eePORT;
               lnFileSize = gnSegmentSize[ leSize ];
            }

            lpAddr = memMap( ( unsigned int ) lpAddr, leSize, leProcessor );

            if ( aeClear == eeTrue )
            {
               for ( lnIndex = 0; lnIndex < lnFileSize; lnIndex++ )
               {
                  lpAddr[ lnIndex ] = 0;
               }
            }
         }
      }

      closedir( lpDir );

      return 0;
   }
   else
   {
      perror( "memInit chdir" );
      return gnError;
   }
}


void * memMap( unsigned int anAddress, teSize aeSize, teProcessor aeProcessor )
{
   char *          lpFileName  = "/mem#########@.dat";
   void *          lpMap;
   int             lnFileFlags = O_RDONLY;
   int             lnFD;
   int             lnCount;
   teBool          leFound     = eeFalse;
   int             lnMode      = 0444;
   int             lnSegmentSize;
   char            lnMemID;
   int             lnOffset = 0;
   teProcessor     leMapProc = aeProcessor;
   unsigned int    lnAddress = anAddress;
   unsigned int    lnBaseAddr;
   unsigned char * lpHashAddr;
   unsigned char   lnHash;


   lnBaseAddr = lnAddress & highMask( gnSegmentSize[ aeSize ] );

   lpHashAddr = ( unsigned char * ) & lnBaseAddr;

   lnHash = ( aeSize + leMapProc + lpHashAddr[ 0 ] + lpHashAddr[ 1 ] +
      + ( lpHashAddr[ 1 ] >> 2 ) + ( lpHashAddr[ 2 ] >> 4 ) )
      & lowMask( gnMaxSegments );

   for ( lnCount = 0; lnCount < gnMaxSegments; lnCount++ )
   {
      if (  gsMemInfo[ lnHash ].meUsed )
      {
         lnSegmentSize = gsMemInfo[ lnHash ].mnSize;

         if ( ( ( void * ) ( lnBaseAddr ) == gsMemInfo[ lnHash ].mnBase ) &&
           ( gsMemInfo[ lnHash ].meProcessor == leMapProc ) )
         {
            lpMap = gsMemInfo[ lnHash ].mnMap +
              ( ( lnAddress & lowMask( lnSegmentSize ) ) + lnOffset);

            leFound = eeTrue;

            break;
         }

         lnHash++;
      }
      else
      {
         break;
      }
   }

   if ( ! leFound )
   {
      if ( gnSegments >= gnMaxSegments )
      {
         fprintf( stderr, "Insufficient Memory segments available\n" );

         return ( void * ) gnError;
      }
      else
      {
         lnSegmentSize = gnSegmentSize[ aeSize ];

         if ( aeProcessor == eeSTBD )
         {
            lnMemID = 's';
         }
         else
         {
            lnMemID = 'p';
	 }


         lpFileName = malloc( strlen( gpPath ) + strlen( lpFileName ) + 1 );

         if ( lpFileName == NULL )
         {
            fprintf( stderr, "Out of memory\n" );

            return ( void * ) gnError;
         }

         sprintf( lpFileName, "%s/mem%c%08x%c.dat", gpPath, lnMemID,
           lnAddress & highMask( lnSegmentSize ), gaSizeID[ aeSize ] );

         lnFD = open( lpFileName, lnFileFlags, lnMode );

         if ( lnFD == gnError )
         {
            if ( errno == ENOENT || errno == EBADF )
            {
               lnFD = creat( lpFileName, lnMode );

               if ( lnFD == gnError )
               {
                  free( lpFileName );

                  perror( "memMap creat" );

                  return ( void * ) gnError;
               }
            }
            else
            {
	       free( lpFileName );
               perror( "memMap open" );

               return ( void * ) gnError;
            }
         }

         close( lnFD );

         sharedMemInit( & gsMemInfo[ lnHash ].msSharedMem );

         if ( sharedMemCreate( & gsMemInfo[ lnHash ].msSharedMem,
           lpFileName, lnSegmentSize ) )
         {
            free( lpFileName );

            return ( void * ) gnError;
         }

         free( lpFileName );

         lpMap = ( void * ) gsMemInfo[ lnHash ].msSharedMem.mpLoc;

         gsMemInfo[ lnHash ].mnBase =
           ( void * ) ( lnAddress & highMask( lnSegmentSize ) );

         gsMemInfo[ lnHash ].mnMap = lpMap;

         gsMemInfo[ lnHash ].mnSize = lnSegmentSize;

         gsMemInfo[ lnHash ].meProcessor = leMapProc;

         gsMemInfo[ lnHash ].meUsed = eeTrue;

         gnSegments++;
      }

      lpMap += ( lnAddress & lowMask( lnSegmentSize ) ) + lnOffset;

      close( lnFD );
   }

   return lpMap;
}

void * memRevMap( unsigned int anAddress )
{
   void *      lpRevMap    = 0;
   teBool      leFound     = eeFalse;
   int         lnSegmentSize;
   int         lnCount;
   teProcessor    leProcessor;

   for ( lnCount = 0; lnCount < gnMaxSegments; lnCount++ )
   {
      if (  gsMemInfo[ lnCount ].meUsed )
      {
	  leProcessor = gsMemInfo[ lnCount ].meProcessor;

	  lnSegmentSize = gsMemInfo[ lnCount ].mnSize;

         if ( ( anAddress >= ( unsigned int ) gsMemInfo[ lnCount ].mnMap ) &&
              ( anAddress < ( unsigned int ) gsMemInfo[ lnCount ].mnMap +
              ( unsigned int ) lnSegmentSize ) )
         {
            lpRevMap = gsMemInfo[ lnCount ].mnBase +
              ( ( anAddress - ( unsigned int )gsMemInfo[ lnCount ].mnMap ) &
              lowMask( gsMemInfo[ lnCount ].mnSize ) );

            leFound = eeTrue;

            break;
         }
      }
   }

   if ( ! leFound )
   {
      fprintf( stderr, "Invalid mapped memory address\n" );
   }

   return lpRevMap;
}


int memRemove( void )
{
   struct dirent * lpEntry;
   DIR *           lpDir;
   char *          lpFileName  = "mem#########@.dat";

   if ( chdir( gpPath ) == 0 )
   {
      lpDir = opendir( gpPath );

      while ( ( lpEntry = readdir( lpDir ) ) != NULL )
      {
         if ( matchName( lpFileName, lpEntry->d_name ) )
         {
	      sharedMemRemove( lpEntry->d_name );
         }
      }

      closedir( lpDir );

      gnSegments = 0;

      return 0;
   }
   else
   {
      perror( "memInit chdir" );

      return gnError;
   }
}

void memSetPath( char * apPath )
{
   char * lpPath;

   lpPath = malloc( strlen( apPath ) + 1 );

   if ( lpPath == NULL )
   {
      fprintf( stderr, "Out of memory\n" );
   }
   else
   {
      gpPath = lpPath;

      strcpy( gpPath, apPath );
   }
}


char * memGetPath( void )
{
   return gpPath;
}
