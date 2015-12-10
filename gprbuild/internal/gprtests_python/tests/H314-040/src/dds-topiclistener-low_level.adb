
pragma Ada_05;

with DDS.Topic;
with DDS.Topic_Impl;

package body DDS.TopicListener.Low_Level is

   ---------------------------
   -- On_Inconsistent_Topic --
   ---------------------------

   procedure On_Inconsistent_Topic
     (Listener : DDS.Listener.Ref_Access;
      C_Topic  : System.Address;
      Status   : in DDS.InconsistentTopicStatus) is
      Topic : DDS.Topic.Ref_Access;
      T_Listener : constant DDS.TopicListener.Ref_Access :=
        DDS.TopicListener.Ref_Access (Listener);
   begin
      Topic := DDS.Topic.Ref_Access
        (DDS.Topic_Impl.Get_FacadeI (C_Topic));
      T_Listener.On_Inconsistent_Topic (Topic, Status);
   end On_Inconsistent_Topic;

end DDS.TopicListener.Low_Level;
