pragma Ada_05;
--  This package presents an ionterface that makes it possible to register
--  natvie threads to the Ada-runtimes.
with System;
package DDS.DomainParticipantFactory.ThreadListners is

   type Thread_Listener_On_Started_Callback is
     access procedure (OnStartedParam : System.Address;
                       Worker         : not null access Integer);

   type Thread_Listener_On_Stopped_Callback is
     access procedure (OnStoppedParam : System.Address;
                       Worker         : not null access Integer);


   procedure On_Thread_Started_Callback (OnStartedParam : System.Address;
                                         Worker         : not null access Integer);

   procedure On_Thread_Stopped_Callback (OnStartedParam : System.Address;
                                         Worker         : not null access Integer);
   type Thread_Listener_I is record
      OnStarted      : Thread_Listener_On_Started_Callback := On_Thread_Started_Callback'Access;
      OnStartedParam : System.Address := System.Null_Address;
      OnStopped      : Thread_Listener_On_Stopped_Callback := On_Thread_Stopped_Callback'Access;
      OnStoppedParam : System.Address := System.Null_Address;
   end record;

   pragma Convention (C, Thread_Listener_I);

   procedure Set_Thread_ListenerI
     (ImplementationParticipant     : System.Address;
      Listener                      : not null access constant Thread_Listener_I);
   pragma Import (C, Set_Thread_ListenerI, "DDS_DomainParticipantFactory_set_thread_listenerI");

end DDS.DomainParticipantFactory.ThreadListners;
