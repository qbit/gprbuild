pragma Ada_05;

with DDS.DomainParticipantListener;
with DDS.TopicListener;
with DDS.SubscriberListener;
with DDS.PublisherListener;
with DDS.Publisher;
with DDS.Topic;
with DDS.MultiTopic;
with DDS.TopicDescription;
with DDS.Subscriber;
with DDS.ContentFilteredTopic;
with DDS.DomainParticipant;

with DDS.Entity_Impl;

package DDS.DomainParticipant_Impl is

   type Ref is new DDS.Entity_Impl.Ref and DDS.DomainParticipant.Ref with null record;
   type Ref_Access is access all Ref'Class;

   function Create_Publisher
     (Self       : not null access Ref;
      Qos        : in DDS.PublisherQos;
      A_Listener : in DDS.PublisherListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Publisher.Ref_Access;

   procedure Delete_Publisher
     (Self      : not null access Ref;
      Publisher : in out DDS.Publisher.Ref_Access);


   function Create_Subscriber
     (Self       : not null access Ref;
      Qos        : in DDS.SubscriberQos;
      A_Listener : in DDS.SubscriberListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Subscriber.Ref_Access;


   procedure Delete_Subscriber
     (Self :  not null access Ref;
      S    :  in out DDS.Subscriber.Ref_Access);

   function Get_Builtin_Subscriber
     (Self :  not null access Ref)
      return DDS.Subscriber.Ref_Access;


   function Create_Topic
     (Self       : not null access Ref;
      Topic_Name : in DDS.String;
      Type_Name  : in DDS.String;
      Qos        : in DDS.TopicQos;
      A_Listener : in DDS.TopicListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Topic.Ref_Access;

   procedure Delete_Topic
     (Self    : not null access Ref;
      A_Topic : in out DDS.Topic.Ref_Access);


   function Find_Topic
     (Self       : not null access Ref;
      Topic_Name : in DDS.String;
      Timeout    : in DDS.Duration_T)
      return DDS.Topic.Ref_Access;


   function Lookup_Topicdescription
     (Self : not null access Ref;
      Name : in DDS.String)
      return DDS.TopicDescription.Ref_Access;


--     function Create_Contentfilteredtopic
--       (Self                   :  access Ref;
--        Name                  : in DDS.String;
--        Related_Topic         : access constant DDS.Topic.Ref'Class;
--        Filter_Expression     : in DDS.String;
--        Expression_Parameters : in DDS.StringSeq)
--        return access DDS.ContentFilteredTopic.Ref'Class;

   procedure Delete_Contentfilteredtopic
     (Self                   : not null access Ref;
      A_Contentfilteredtopic : not null DDS.ContentFilteredTopic.Ref_Access);


--     function Create_Multitopic
--       (Self                    :  access Ref;
--        Name                    : in DDS.String;
--        Type_Name               : in DDS.String;
--        Subscription_Expression : in DDS.String;
--        Expression_Parameters   : in DDS.StringSeq)
--        return access DDS.MultiTopic.Ref'Class;


   procedure Delete_Multitopic
     (Self         : not null access Ref;
      A_Multitopic : not null DDS.MultiTopic.Ref_Access);


   procedure Delete_Contained_Entities
     (Self :  not null access Ref);


   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DomainParticipantQos);


   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DomainParticipantQos);


   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : DDS.DomainParticipantListener.Ref_Access;
      Mask       : in DDS.StatusMask);

   function Get_Listener
     (Self :  not null access Ref)
      return DDS.DomainParticipantListener.Ref_Access;


   procedure Ignore_Participant
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T);


   procedure Ignore_Topic
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T);

   procedure Ignore_Publication
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T);

   procedure Ignore_Subscription
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T);

   function Get_Domain_Id
     (Self :  not null access Ref)
      return DDS.DomainId_T;

   procedure Assert_Liveliness
     (Self :  not null access Ref);

   procedure Set_Default_Publisher_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos);

   procedure Get_Default_Publisher_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.PublisherQos);

   procedure Set_Default_Subscriber_Qos
     (Self : not null access Ref;
      Qos  : in DDS.SubscriberQos);

   procedure Get_Default_Subscriber_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.SubscriberQos);


   procedure Set_Default_Topic_Qos
     (Self : not null access Ref;
      Qos  : in DDS.TopicQos);


   procedure Get_Default_Topic_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.TopicQos);


--     procedure Get_Discovered_Participants
--       (Self                :  access Ref;
--        Participant_Handles :  access DDS.InstanceHandleSeq;
--        Returns             : out DDS.ReturnCode_T);


   procedure Get_Discovered_Participant_Data
     (Self               : not null access Ref;
      Participant_Handle : in DDS.InstanceHandle_T;
      Participant_Data   : not null DDS.ParticipantBuiltinTopicData_Access);


--     procedure Get_Discovered_Topics
--       (Self          :  access Ref;
--        Topic_Handles :  access DDS.InstanceHandleSeq;
--        Returns       : out DDS.ReturnCode_T);


   procedure Get_Discovered_Topic_Data
     (Self         : not null access Ref;
      Topic_Handle : in DDS.InstanceHandle_T;
      Topic_Data   : not null DDS.TopicBuiltinTopicData_Access);


   function Contains_Entity
     (Self     : not null access Ref;
      A_Handle : in DDS.InstanceHandle_T)
      return Boolean;

   procedure Get_Current_Time
     (Self         : not null access Ref;
      Current_Time : in out DDS.Time_T);

   procedure Free (This : in out Ref_Access);

   function CreateI
     (C_Participant_Factory : in System.Address;
      Domain_Id             : in DDS.DomainId_T;
      Qos                   : in DDS.DomainParticipantQos;
      A_Listener            : in DDS.DomainParticipantListener.Ref_Access;
      Mask                  : in DDS.StatusMask)
     return DDS.DomainParticipant.Ref_Access;

   function Get_FacadeI (C_DomainParticpant : System.Address)
                        return Ref_Access;

private

   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

end DDS.DomainParticipant_Impl;
