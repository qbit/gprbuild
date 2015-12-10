pragma Ada_05;

pragma Style_Checks ("NM32766");

with DDS.Domain_Entity_Impl;
with DDS.Publisher;
limited with DDS.DomainParticipant;
with DDS.PublisherListener;
with DDS.Topic;
with DDS.DataWriter;
with DDS.DataWriterListener;

with Ada.Unchecked_Deallocation;

package DDS.Publisher_Impl is

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.Publisher.Ref with null record;
   type Ref_Access is access all Ref'Class;

   --

   function Create_DataWriter
     (Self       : not null access Ref;
      A_Topic    : in DDS.Topic.Ref_Access;
      Qos        : in DDS.DataWriterQos;
      A_Listener : in DDS.DataWriterListener.Ref_Access;
      Mask       : in DDS.StatusMask)
     return DDS.DataWriter.Ref_Access;


   procedure Delete_DataWriter
     (Self         : not null access Ref;
      A_DataWriter : in out DDS.DataWriter.Ref_Access);

   function Lookup_DataWriter
     (Self       : not null access Ref;
      Topic_Name : in DDS.String_Ptr)
     return DDS.DataWriter.Ref_Access;

   function Delete_Contained_Entities
     (Self : not null access Ref)
     return DDS.ReturnCode_T;

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos);

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos);

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.PublisherListener.Ref_Access;
      Mask       : in DDS.StatusMask);

   function Get_Listener
     (Self : not null access Ref)
      return DDS.PublisherListener.Ref_Access;

   function Suspend_Publications
     (Self : not null access Ref)
      return DDS.ReturnCode_T;

   function Resume_Publications
     (Self : not null access Ref)
      return DDS.ReturnCode_T;

   function Begin_Coherent_Changes
     (Self : not null access Ref)
      return DDS.ReturnCode_T;

   function End_Coherent_Changes
     (Self : not null access Ref)
      return DDS.ReturnCode_T;

   function Wait_For_Acknowledgments
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
      return DDS.ReturnCode_T;

   function Get_Participant
     (Self : not null access Ref)
      return access DDS.DomainParticipant.Ref'Class;

   procedure Set_Default_DataWriter_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataWriterQos);

   procedure Get_Default_DataWriter_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataWriterQos);

   procedure Copy_From_Topic_Qos
     (Self             : not null access Ref;
      A_DataWriter_Qos : in out DDS.DataWriterQos;
      A_Topic_Qos      : in DDS.TopicQos);

   -- Impl only

   procedure Free (This : in out Ref_Access);

   function CreateI (C_Participant : in System.Address;
                     Qos           : in DDS.PublisherQos;
                     A_Listener    : in DDS.PublisherListener.Ref_Access;
                     Mask          : in DDS.StatusMask)
                    return DDS.Publisher.Ref_Access;

   function Get_FacadeI (C_Publisher : System.Address)
                        return Ref_Access;

private

   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

end DDS.Publisher_Impl;
