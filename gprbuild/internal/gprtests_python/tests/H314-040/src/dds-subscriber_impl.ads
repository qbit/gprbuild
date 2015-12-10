pragma Ada_05;

with DDS.Subscriber;
limited with DDS.DomainParticipant;
with DDS.SubscriberListener;
with DDS.DataReaderListener;
with DDS.Topic;
with DDS.Domain_Entity_Impl;
with DDS.DataReader;
with DDS.DataReaderSeq;

private with System;
private with Ada.Unchecked_Deallocation;

package DDS.Subscriber_Impl is

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.Subscriber.Ref with private;
   type Ref_Access is access all Ref'Class;

   --

   function Create_DataReader
     (Self       : not null access Ref;
      A_Topic    : not null DDS.Topic.Ref_Access;
      Qos        : in DDS.DataReaderQos;
      A_Listener : in DDS.DataReaderListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.DataReader.Ref_Access;


   procedure Delete_DataReader
     (Self         : not null access Ref;
      A_DataReader : in out DDS.DataReader.Ref_Access);

   procedure Delete_Contained_Entities
     (Self : not null access Ref);

   function Lookup_DataReader
     (Self       : not null access Ref;
      Topic_Name : not null DDS.String_Ptr)
      return DDS.DataReader.Ref_Access;

   procedure Get_DataReaders
     (Self            : not null access Ref;
      Readers         : not null DDS.DataReaderSeq.Ref_Access;
      Sample_States   : in DDS.SampleStateKind_Access;
      View_States     : in DDS.ViewStateKind_Access;
      Instance_States : in DDS.InstanceStateKind_Access);

   procedure Notify_DataReaders
     (Self : not null access Ref);

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.SubscriberQos);

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.SubscriberQos);

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : not null DDS.SubscriberListener.Ref_Access;
      Mask       : in DDS.StatusKind);

   function Get_Listener
     (Self : not null access Ref)
      return DDS.SubscriberListener.Ref_Access;

   procedure Begin_Access
     (Self : not null access Ref);

   procedure End_Access
     (Self : not null access Ref);

   function Get_Participant
     (Self : not null access Ref)
      return access DDS.DomainParticipant.Ref'Class;

   procedure Set_Default_DataReader_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataReaderQos);

   procedure Get_Default_DataReader_Qos
     (Self  : not null access Ref;
      Qos   : in out DDS.DataReaderQos);

   procedure Copy_From_Topic_Qos
     (Self             : not null access Ref;
      A_DataReader_Qos : in out DDS.DataReaderQos;
      A_Topic_Qos      : in DDS.TopicQos);

   procedure Free (This : in out Ref_Access);

   function CreateI
     (C_Participant : in System.Address;
      Qos           : in DDS.SubscriberQos;
      A_Listener    : in DDS.SubscriberListener.Ref_Access;
      Mask          : in DDS.StatusMask)
     return DDS.Subscriber.Ref_Access;

   function Get_FacadeI (C_Subscriber : System.Address)
                        return Ref_Access;

private

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.Subscriber.Ref with null record;
   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

end DDS.Subscriber_Impl;
