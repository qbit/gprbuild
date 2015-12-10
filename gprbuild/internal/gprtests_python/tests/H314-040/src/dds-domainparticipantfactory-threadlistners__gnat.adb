pragma Ada_05;
with GNAT.Threads;
package body DDS.DomainParticipantFactory.ThreadListners is
--  The routenes below is specific for GNAT and are used
--  to register threads in the midleware to the runtimes.

   procedure On_Thread_Started_Callback (OnStartedParam : System.Address;
                                         Worker         : not null access Integer) is
      pragma Unreferenced (OnStartedParam, Worker);
      Dummy : System.Address;
      pragma Unreferenced (Dummy);
   begin
      Dummy := GNAT.Threads.Register_Thread;
   end On_Thread_Started_Callback;

   procedure On_Thread_Stopped_Callback (OnStartedParam : System.Address;
                                         Worker         : not null access Integer)is
      pragma Unreferenced (OnStartedParam, Worker);
   begin
      GNAT.Threads.Unregister_Thread;
   end On_Thread_Stopped_Callback;

end DDS.DomainParticipantFactory.ThreadListners;
