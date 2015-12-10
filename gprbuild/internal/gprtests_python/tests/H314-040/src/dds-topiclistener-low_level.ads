pragma Ada_05;

with DDS.Listener.Low_Level;
with System;

package DDS.TopicListener.Low_Level is

   type T_On_Inconsistent_Topic is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Topic       : System.Address;
      Status        : in DDS.InconsistentTopicStatus);
   pragma Convention (C, T_On_Inconsistent_Topic);


   procedure On_Inconsistent_Topic
     (Listener : DDS.Listener.Ref_Access;
      C_Topic  : System.Address;
      Status   : in DDS.InconsistentTopicStatus);
   pragma Convention (C, On_Inconsistent_Topic);

   type C_TopicListener is record
      Listener            : DDS.Listener.Low_Level.C_Listener;
      Inconsistent_Topic  : T_On_Inconsistent_Topic;
   end record;
   pragma Convention (C, C_TopicListener);

   C_TopicListener_DEFAULT : constant C_TopicListener :=
     (Listener            => (Listener_Data => System.Null_Address),
      Inconsistent_Topic  => On_Inconsistent_Topic'Access);

end DDS.TopicListener.Low_Level;
