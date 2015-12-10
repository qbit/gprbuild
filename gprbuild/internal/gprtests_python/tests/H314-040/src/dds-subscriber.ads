pragma Ada_05;

limited with DDS.DomainParticipant;
with DDS.SubscriberListener;
with DDS.DataReaderListener;
with DDS.Topic;
with DDS.Domain_Entity;
with DDS.DataReader;
with DDS.DataReaderSeq;

package DDS.Subscriber is

   DATAREADER_QOS_DEFAULT : DataReaderQos;
   DATAREADER_QOS_USE_TOPIC_QOS : DataReaderQos;

   type Ref is interface and DDS.Domain_Entity.Ref;
   type Ref_Access is access all Ref'Class;

   function Create_DataReader
     (Self       : not null access Ref;
      A_Topic    : not null DDS.Topic.Ref_Access;
      Qos        : in DDS.DataReaderQos;
      A_Listener : in DDS.DataReaderListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.DataReader.Ref_Access is abstract;

   procedure Delete_DataReader
     (Self         : not null access Ref;
      A_DataReader : in out DDS.DataReader.Ref_Access) is abstract;

   procedure Delete_Contained_Entities
     (Self : not null access Ref) is abstract;

   function Lookup_DataReader
     (Self       : not null access Ref;
      Topic_Name : not null DDS.String_Ptr)
      return DDS.DataReader.Ref_Access is abstract;

   procedure Get_DataReaders
     (Self            : not null access Ref;
      Readers         : not null DDS.DataReaderSeq.Ref_Access;
      Sample_States   : in DDS.SampleStateKind_Access;
      View_States     : in DDS.ViewStateKind_Access;
      Instance_States : in DDS.InstanceStateKind_Access) is abstract;

   procedure Notify_DataReaders
     (Self : not null access Ref) is abstract;

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.SubscriberQos) is abstract;

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.SubscriberQos) is abstract;

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : not null DDS.SubscriberListener.Ref_Access;
      Mask       : in       DDS.StatusKind) is abstract;

   function Get_Listener
     (Self : not null access Ref)
      return DDS.SubscriberListener.Ref_Access is abstract;

   procedure Begin_Access
     (Self : not null access Ref) is abstract;

   procedure End_Access
     (Self : not null access Ref) is abstract;

   function Get_Participant
     (Self : not null access Ref)
      return access DDS.DomainParticipant.Ref'Class is abstract;

   procedure Set_Default_DataReader_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataReaderQos) is abstract;

   procedure Get_Default_DataReader_Qos
     (Self  : not null access Ref;
      Qos   : in out DDS.DataReaderQos) is abstract;

   procedure Copy_From_Topic_Qos
     (Self           : not null access Ref;
      A_DataReader_Qos : in out DDS.DataReaderQos;
      A_Topic_Qos      : in DDS.TopicQos) is abstract;

private

   procedure Initialize;

end DDS.Subscriber;
