pragma Ada_05;

pragma Style_Checks ("NM32766");

limited with DDS.DomainParticipant;

with DDS.PublisherListener;
with DDS.DataWriterListener;
with DDS.Topic;
with DDS.DataWriter;
with Dds.Domain_Entity;

package DDS.Publisher is

   DATAWRITER_QOS_DEFAULT : DataWriterQos;
   DATAWRITER_QOS_USE_TOPIC_QOS : DataWriterQos;

   type Ref is Interface and DDS.Domain_Entity.Ref;
   type Ref_Access is access all Ref'Class;

   function Create_DataWriter
     (Self       : not null access Ref;
      A_Topic    : in DDS.Topic.Ref_Access;
      Qos        : in DDS.DataWriterQos;
      A_Listener : in DDS.DataWriterListener.Ref_Access;
      Mask       : in DDS.StatusMask)
     return DDS.DataWriter.Ref_Access is abstract;

   procedure Delete_DataWriter
     (Self         : not null access Ref;
      A_DataWriter : in out DDS.DataWriter.Ref_Access) is abstract;

   function Lookup_DataWriter
     (Self       : not null access Ref;
      Topic_Name : in DDS.String_Ptr)
     return DDS.DataWriter.Ref_Access is abstract;

   function Delete_Contained_Entities
     (Self : not null access Ref)
     return DDS.ReturnCode_T is abstract;

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos) is abstract;

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos) is abstract;

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.PublisherListener.Ref_Access;
      Mask       : in DDS.StatusMask) is abstract;

   function Get_Listener
     (Self : not null access Ref)
      return DDS.PublisherListener.Ref_Access is abstract;

   function Suspend_Publications
     (Self : not null access Ref)
      return DDS.ReturnCode_T is abstract;

   function Resume_Publications
     (Self : not null access Ref)
      return DDS.ReturnCode_T is abstract;

   function Begin_Coherent_Changes
     (Self : not null access Ref)
      return DDS.ReturnCode_T is abstract;

   function End_Coherent_Changes
     (Self : not null access Ref)
      return DDS.ReturnCode_T is abstract;

   function Wait_For_Acknowledgments
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
      return DDS.ReturnCode_T is abstract;

   function Get_Participant
     (Self : not null access Ref)
      return access DDS.DomainParticipant.Ref'Class is abstract;

   procedure Set_Default_DataWriter_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataWriterQos) is abstract;

   procedure Get_Default_DataWriter_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataWriterQos) is abstract;

   procedure Copy_From_Topic_Qos
     (Self               : not null access Ref;
      A_DataWriter_Qos   : in out DDS.DataWriterQos;
      A_Topic_Qos        : in DDS.TopicQos) is abstract;

private

   procedure Initialize;

end DDS.Publisher;
