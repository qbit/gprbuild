#ifndef MESSAGE_H
#define MESSAGE_H
   #include <sys/msg.h>
   #include <constants.h>

   typedef struct
   {
      int    mnID;
      teBool meCreated;
      char * mpKey;
   } tsMsg;

   tsMsg * messageCreate( char * apKey );

   int messageSend( tsMsg * apMsg, void * apData, int anSize );

   int messageRecv( tsMsg * apMsg, void * apData, int anSize, teBool aeWait );

   int messageFree( tsMsg * apMsg );

   int messageDelete( tsMsg * apMsg );

   teBool messageCreated( tsMsg * apMsg );

#endif
