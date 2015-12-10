#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <message.h>
#include <memMap.h>
#include <sys/msg.h>

typedef struct
{
  long mnType;
  char maData[ 1 ];
} msData;


tsMsg * messageCreate( char * apKey )
{
  key_t lnKey;

  tsMsg * lpMsg;

  int lnFlags = IPC_CREAT | IPC_EXCL | MSG_R | MSG_W;


  lpMsg = ( tsMsg * ) malloc( sizeof( tsMsg ) );

  if ( lpMsg )
    {
      lpMsg->mnID = gnError;

      lpMsg->meCreated = eeFalse;

      lpMsg->mpKey = malloc( strlen( memGetPath() ) + strlen( apKey ) + 1 );

      if ( lpMsg->mpKey )
	{
	  sprintf( lpMsg->mpKey, "%s%s", memGetPath(), apKey );

	  lnKey = ftok( lpMsg->mpKey, 'W' );

	  if ( lnKey == gnError )
	    {
	      fprintf( stderr, "ftok error: %s for %s\n", strerror( errno ), lpMsg->mpKey );

	      free( lpMsg->mpKey );

	      free( lpMsg );

	      return 0;
	    }

	  lpMsg->mnID = msgget( lnKey, lnFlags );

	  if ( lpMsg->mnID == gnError )
	    {
	      if ( errno == EEXIST || EBADF )
		{
		  lnFlags = IPC_CREAT | MSG_R | MSG_W;

		  lpMsg->mnID = msgget( lnKey, lnFlags );

		  if ( lpMsg->mnID == gnError )
		    {
		      fprintf( stderr, "msgget error 2: %s\n", strerror( errno ) );

		      free( lpMsg->mpKey );

		      free( lpMsg );

		      return 0;
		    }

		  errno = 0;
		}
	      else
		{
		  fprintf( stderr, "msgget error 1: %s\n", strerror( errno ) );

		  free( lpMsg->mpKey );

		  free( lpMsg );

		  return 0;
		}
	    }

	  lpMsg->meCreated = eeTrue;

	  return lpMsg;
	}
      else
	{
	  fprintf( stderr, "messageCreate malloc failed for key\n" );

	  free( lpMsg );

	  return 0;
	}
    }
  else
    {
      fprintf( stderr, "messageCreate malloc failed for object\n" );

      return 0;
    }
}


int messageFree( tsMsg * apMsg )
{
  if ( apMsg->mnID != gnError )
    {
      if ( msgctl( apMsg->mnID, IPC_RMID, null ) == gnError )
	{
	  fprintf( stderr, "msgctl error 2: %s\n", strerror( errno ) );

	  return gnError;
	}

      apMsg->meCreated = eeFalse;

      return 0;
    }
  else
    {
      fprintf( stderr, "msgFree error: invalid message queue ID\n" );

      return gnError;
    }
}


int messageSend( tsMsg * apMsg, void * apData, int anSize )
{
  int lnMsgFlag = IPC_NOWAIT;

  msData * lpData;

  int lnStatus;

  if ( apMsg->mnID != gnError )
    {
      lpData = ( msData * ) malloc( anSize + sizeof( msData ) - 1 );

      if ( lpData )
	{
	  lpData->mnType = 1;

	  memcpy( lpData->maData, apData, anSize );

	  errno = 0;

	  lnStatus = msgsnd( apMsg->mnID, ( void * ) lpData, anSize, lnMsgFlag );

	  if ( ( lnStatus == gnError ) && errno )
	    {
	      fprintf( stderr, "msgsnd error: %s\n", strerror( errno ) );

	      free( lpData );

	      return gnError;
	    }

	  free( lpData );
	}
      else
	{
	  fprintf( stderr, "msgSend error: %s\n", strerror( errno ) );

	  return gnError;
	}

      return 0;
    }
  else
    {
      fprintf( stderr, "msgSend error: invalid message queue ID\n" );

      return gnError;
    }
}


int messageRecv( tsMsg * apMsg, void * apData, int anSize, teBool aeWait )
{
  msData * lpData;

  int lnMsgFlag = 0; /* Wait to receive a message */

  int lnType = 0; /* Select all messages */

  int lnStatus;

  if ( aeWait )
    {
      lnMsgFlag = 0;
    }
  else
    {
      lnMsgFlag = IPC_NOWAIT;
    }

  if ( apMsg->mnID != gnError )
    {
      lpData = ( msData * ) malloc( anSize + sizeof( msData ) - 1 );

      if ( lpData )
	{
	  lnStatus = msgrcv( apMsg->mnID, lpData, anSize, lnType, lnMsgFlag );

	  if ( lnStatus == gnError )
	    {
	      if ( errno == ENOMSG )
		{
		  free( lpData );

		  return gnEmpty;
		}
	      else
		{
		  fprintf( stderr, "msgrcv error: %s\n", strerror( errno ) );

		  free( lpData );

		  return gnError;
		}
	    }

	  memcpy( apData, lpData->maData, anSize );

	  free( lpData );
	}
      else
	{
	  fprintf( stderr, "msgRecv error: %s\n", strerror( errno ) );

	  return gnError;
	}

      return 0;
    }
  else
    {
      fprintf( stderr, "msgRecv error: invalid message queue ID\n" );

      return gnError;
    }
}


int messageDelete( tsMsg * apMsg )
{
  key_t lnKey;

  int lnFlags = IPC_EXCL | MSG_R | MSG_W;

  int lnID;


  if ( apMsg )
    {
      if ( apMsg->mpKey )
	{
	  lnKey = ftok( apMsg->mpKey, 'W' );

	  if ( lnKey == gnError )
	    {
	      /* OK if key file does not exist */
	      if ( errno == ENOENT || errno == EBADF )
		{
		  free( apMsg->mpKey );

		  free( apMsg );

		  return 0;
		}
	      else
		{
		  fprintf( stderr, "ftok error: %s for %s\n", strerror( errno ), apMsg->mpKey );

		  free( apMsg->mpKey );

		  free( apMsg );

		  return gnError;
		}
	    }

	  lnID = msgget( lnKey, lnFlags );

	  if ( lnID == gnError )
	    {
	      if ( errno != ENOENT )
		{
		  fprintf( stderr, "msgget error: %s\n", strerror( errno ) );

		  free( apMsg->mpKey );

		  free( apMsg );

		  return gnError;
		}
	    }
	  else
	    {
	      if ( msgctl( lnID, IPC_RMID, null ) == gnError )
		{
		  free( apMsg->mpKey );

		  free( apMsg );

		  fprintf( stderr, "msgctl error: %s\n", strerror( errno ) );

		  return gnError;
		}
	    }

	  free( apMsg->mpKey );

	  free( apMsg );

	  return 0;
	}
      else
	{
	  fprintf( stderr, "messageDelete error: invalid key\n" );

	  free( apMsg );

	  return gnError;
	}
    }
  else
    {
      fprintf( stderr, "messageDelete error: invalid object\n" );

      return gnError;
    }
}


teBool messageCreated( tsMsg * apMsg )
{
  return apMsg->meCreated;
}
