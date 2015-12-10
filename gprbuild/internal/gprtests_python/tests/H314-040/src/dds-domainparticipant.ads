pragma Ada_05;

with DDS.StatusCondition;
with DDS.DomainParticipantListener;
with DDS.TopicListener;
with DDS.SubscriberListener;
with DDS.PublisherListener;
with DDS.Publisher;
with DDS.Topic;
with DDS.Domain_Entity;
with DDS.MultiTopic;
with DDS.TopicDescription;
with DDS.Subscriber;
with DDS.ContentFilteredTopic;

package DDS.DomainParticipant is

   TOPIC_QOS_DEFAULT : TopicQos;
   PUBLISHER_QOS_DEFAULT : PublisherQos;
   SUBSCRIBER_QOS_DEFAULT : SubscriberQos;

   type Ref is limited interface and DDS.Domain_Entity.Ref;
   type Ref_Access is access all Ref'Class;

   --  Re-Implement From DDS.DomainEntity

   procedure Enable (Self : not null access Ref) is abstract;

   function Get_StatusCondition (Self : not null access Ref) return
     DDS.StatusCondition.Ref_Access is abstract;

   function Get_Status_Changes (Self : not null access Ref) return
     DDS.StatusMask is abstract;

   function Get_Instance_Handle (Self : not null access Ref) return
     DDS.InstanceHandle_T is abstract;

   --

   function Create_Publisher
     (Self       : not null access Ref;
      Qos        : in DDS.PublisherQos;
      A_Listener : in DDS.PublisherListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Publisher.Ref_Access is abstract;

   procedure Delete_Publisher
     (Self      : not null access Ref;
      Publisher : in out DDS.Publisher.Ref_Access) is abstract;


   function Create_Subscriber
     (Self       : not null access Ref;
      Qos        : in DDS.SubscriberQos;
      A_Listener : in DDS.SubscriberListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Subscriber.Ref_Access is abstract;


   procedure Delete_Subscriber
     (Self :  not null access Ref;
      S    :  in out DDS.Subscriber.Ref_Access) is abstract;


   function Get_Builtin_Subscriber
     (Self :  not null access Ref)
      return DDS.Subscriber.Ref_Access is abstract;


   function Create_Topic
     (Self       : not null access Ref;
      Topic_Name : in DDS.String;
      Type_Name  : in DDS.String;
      Qos        : in DDS.TopicQos;
      A_Listener : in DDS.TopicListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Topic.Ref_Access is abstract;


   procedure Delete_Topic
     (Self    : not null access Ref;
      A_Topic : in out DDS.Topic.Ref_Access) is abstract;


   function Find_Topic
     (Self       : not null access Ref;
      Topic_Name : in DDS.String;
      Timeout    : in DDS.Duration_T)
      return DDS.Topic.Ref_Access is abstract;


   function Lookup_Topicdescription
     (Self : not null access Ref;
      Name : in DDS.String)
      return DDS.TopicDescription.Ref_Access is abstract;


   --     function Create_Contentfilteredtopic
   --       (Self                   :  access Ref;
   --        Name                  : in DDS.String;
   --        Related_Topic         : access constant DDS.Topic.Ref'Class;
   --        Filter_Expression     : in DDS.String;
   --        Expression_Parameters : in DDS.StringSeq)
   --        return access DDS.ContentFilteredTopic.Ref'Class is abstract;

   procedure Delete_Contentfilteredtopic
     (Self                   : not null access Ref;
      A_Contentfilteredtopic : not null DDS.ContentFilteredTopic.Ref_Access) is abstract;


   --     function Create_Multitopic
   --       (Self                    :  access Ref;
   --        Name                    : in DDS.String;
   --        Type_Name               : in DDS.String;
   --        Subscription_Expression : in DDS.String;
   --        Expression_Parameters   : in DDS.StringSeq)
   --        return access DDS.MultiTopic.Ref'Class is abstract;


   procedure Delete_Multitopic
     (Self         : not null access Ref;
      A_Multitopic : not null DDS.MultiTopic.Ref_Access)is abstract;


   procedure Delete_Contained_Entities
     (Self :  not null access Ref) is abstract;


   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DomainParticipantQos) is abstract;


   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DomainParticipantQos) is abstract;


   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : DDS.DomainParticipantListener.Ref_Access;
      Mask       : in DDS.StatusMask) is abstract;


   function Get_Listener
     (Self :  not null access Ref)
      return DDS.DomainParticipantListener.Ref_Access is abstract;


   procedure Ignore_Participant
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T) is abstract;


   procedure Ignore_Topic
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T)is abstract;

   procedure Ignore_Publication
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T)is abstract;

   procedure Ignore_Subscription
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T)is abstract;

   function Get_Domain_Id
     (Self : not null access Ref)
      return DDS.DomainId_T is abstract;

   procedure Assert_Liveliness
     (Self : not null access Ref)is abstract;

   procedure Set_Default_Publisher_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos) is abstract;

   procedure Get_Default_Publisher_Qos
     (Self    :  not null access Ref;
      Qos     :  in out DDS.PublisherQos) is abstract;

   procedure Set_Default_Subscriber_Qos
     (Self : not null access Ref;
      Qos  : in DDS.SubscriberQos) is abstract;

   procedure Get_Default_Subscriber_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.SubscriberQos) is abstract;


   procedure Set_Default_Topic_Qos
     (Self : not null access Ref;
      Qos  : in DDS.TopicQos) is abstract;


   procedure Get_Default_Topic_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.TopicQos) is abstract;


   --     procedure Get_Discovered_Participants
   --       (Self                :  access Ref;
   --        Participant_Handles :  access DDS.InstanceHandleSeq;
   --        Returns             : out DDS.ReturnCode_T) is abstract;


   procedure Get_Discovered_Participant_Data
     (Self               : not null access Ref;
      Participant_Handle : in DDS.InstanceHandle_T;
      Participant_Data   : not null DDS.ParticipantBuiltinTopicData_Access) is abstract;


   --     procedure Get_Discovered_Topics
   --       (Self          :  access Ref;
   --        Topic_Handles :  access DDS.InstanceHandleSeq;
   --        Returns       : out DDS.ReturnCode_T);


   procedure Get_Discovered_Topic_Data
     (Self         : not null access Ref;
      Topic_Handle : in DDS.InstanceHandle_T;
      Topic_Data   : not null DDS.TopicBuiltinTopicData_Access) is abstract;

   function Contains_Entity
     (Self     : not null access Ref;
      A_Handle : in DDS.InstanceHandle_T)
      return Boolean is abstract;

   procedure Get_Current_Time
     (Self         : not null access Ref;
      Current_Time : in out DDS.Time_T) is abstract;

private

   procedure Initialize;

end DDS.DomainParticipant;
