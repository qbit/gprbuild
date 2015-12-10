
With FCAPI;
With FC_Manager;

Procedure Hello is

   StatusInfo   : FCAPI.Fibre_Status_Type;
   Close_Status : FCAPI.Fibre_Transfer_Err;
   Host         : FCAPI.U8;
   Dummy        : FC_Manager.Input_Message_Type;

Begin
   StatusInfo := FCAPI.Null_Fibre_Status;

   Host := 100;

   Close_Status := FCAPI.Fibre_Close
      ( Host_ID   => Host,
        FastClose => 1 );

End Hello;
