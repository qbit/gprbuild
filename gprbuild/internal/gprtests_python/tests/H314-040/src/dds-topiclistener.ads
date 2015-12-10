pragma Ada_05;

with DDS.Listener;
limited with DDS.Topic;

package DDS.TopicListener is

   type Ref is limited interface and DDS.Listener.Ref;
   type Ref_Access is access all Ref'Class;

   procedure On_Inconsistent_Topic
     (Self      : not null access Ref;
      Topic     : access constant DDS.Topic.Ref'Class;
      Status    : in DDS.InconsistentTopicStatus) is null;

end DDS.TopicListener;
