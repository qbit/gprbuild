pragma Ada_05;

with DDS.DataReader;
with DDS.Domain_Entity_Impl;
limited with DDS.Subscriber;
with DDS.Topic;
with DDS.TopicDescription;
with DDS.DataReaderListener;
with DDS.QueryCondition;
with DDS.ReadCondition;

with System;

package DDS.DataReader_Impl is

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.DataReader.Ref with private;
   type Ref_Access is access all Ref'Class;

   function Create_Readcondition
     (Self            : not null access Ref;
      Sample_States   : in DDS.SampleStateMask;
      View_States     : in DDS.ViewStateMask;
      Instance_States : in DDS.InstanceStateMask)
      return DDS.ReadCondition.Ref_Access;

   function Create_Querycondition
     (Self             : not null access Ref;
      Sample_States    : in DDS.SampleStateMask;
      View_States      : in DDS.ViewStateMask;
      Instance_States  : in DDS.InstanceStateMask;
      Query_Expression : in DDS.String;
      Query_Parameters : in DDS.String_Seq.Sequence)
      return DDS.QueryCondition.Ref_Access;

   function Delete_Readcondition
     (Self        : not null access Ref;
      A_Condition : in DDS.ReadCondition.Ref_Access)
      return DDS.ReturnCode_T;

   procedure  Delete_Contained_Entities
     (Self : not null access Ref);
   --  Deletes all the entities that were created by means of the "create" operations on the DDS_DataReader.
   --  Deletes all contained DDS_ReadCondition and DDS_QueryCondition objects.
   --  Once DDS_DataReader_delete_contained_entities returns successfully,
   --  the application may delete the DDS_DataReader, knowing that it has no contained
   --- DDS_ReadCondition and DDS_QueryCondition objects.
   --      rasies exception on error

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataReaderQos);

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataReaderQos);

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DataReaderListener.Ref_Access;
      Mask       : in DDS.StatusMask);

   function Get_Listener
     (Self : not null access Ref)
      return DDS.DataReaderListener.Ref_Access;

   function Get_Topicdescription
     (Self : not null access Ref)
      return DDS.TopicDescription.Ref_Access;

   function Get_Subscriber
     (Self : not null access Ref)
      return access DDS.Subscriber.Ref'Class;

   function Get_Sample_Rejected_Status
     (Self : not null access Ref)
      return DDS.SampleRejectedStatus;

   function Get_Liveliness_Changed_Status
     (Self : not null access Ref)
      return DDS.LivelinessChangedStatus;

   function Get_Requested_Deadline_Missed_Status
     (Self : not null access Ref)
      return DDS.RequestedDeadlineMissedStatus;

   function Get_Requested_Incompatible_Qos_Status
     (Self : not null access Ref)
      return DDS.RequestedIncompatibleQosStatus;

   function Get_Subscription_Match_Status
     (Self : not null access Ref)
      return DDS.SubscriptionMatchedStatus;

   function Get_Sample_Lost_Status
     (Self : not null access Ref)
      return DDS.SampleLostStatus;

   function Wait_For_Historical_Data
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
     return DDS.ReturnCode_T;

   procedure Get_Matched_Publications
     (Self                : not null access Ref;
      Publication_Handles : in out DDS.InstanceHandle_Seq.Sequence;
      Returns             : out DDS.ReturnCode_T);

   procedure Get_Matched_Publication_Data
     (Self               : not null access Ref;
      Publication_Data   : in out DDS.PublicationBuiltinTopicData;
      Publication_Handle : in DDS.InstanceHandle_T;
      Returns            : out DDS.ReturnCode_T);

   --  Impl
   procedure SetSubscriber (Self  : not null access Ref;
                            Subscriber : access DDS.Subscriber.Ref'Class);


   procedure Free (This : in out Ref_Access);

   function CreateI (C_Subscriber : System.Address;
                     A_Topic      : not null DDS.Topic.Ref_Access;
                     Qos          : in DDS.DataReaderQos;
                     A_Listener   : in DDS.DataReaderListener.Ref_Access;
                     Mask         : in DDS.StatusMask)
                    return DDS.DataReader.Ref_Access;

   function Get_FacadeI (C_DataReader : System.Address)
     return Ref_Access;

private

   function Delete_Contained_Entities
     (Self : not null access Ref)
      return DDS.ReturnCode_T;

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.DataReader.Ref with null record;

   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;


end DDS.DataReader_Impl;
