pragma Ada_05;

limited with DDS.Subscriber;
with DDS.TopicDescription;
with DDS.DataReaderListener;
with DDS.QueryCondition;
with DDS.ReadCondition;
with DDS.Domain_Entity;


package DDS.DataReader is

   type Ref is interface and DDS.Domain_Entity.Ref;
   type Ref_Access is access all Ref'Class;

   function Create_Readcondition
     (Self            : not null access Ref;
      Sample_States   : in DDS.SampleStateMask;
      View_States     : in DDS.ViewStateMask;
      Instance_States : in DDS.InstanceStateMask)
      return DDS.ReadCondition.Ref_Access is abstract;

   function Create_Querycondition
     (Self             : not null access Ref;
      Sample_States    : in DDS.SampleStateMask;
      View_States      : in DDS.ViewStateMask;
      Instance_States  : in DDS.InstanceStateMask;
      Query_Expression : in DDS.String;
      Query_Parameters : in DDS.String_Seq.Sequence)
      return DDS.QueryCondition.Ref_Access is abstract;

   function Delete_Readcondition
     (Self        : not null access Ref;
      A_Condition : in DDS.ReadCondition.Ref_Access)
      return DDS.ReturnCode_T is abstract;


   procedure  Delete_Contained_Entities
     (Self : not null access Ref) is abstract;
   --  Deletes all the entities that were created by means of the "create" operations on the DDS_DataReader.
   --  Deletes all contained DDS_ReadCondition and DDS_QueryCondition objects.
   --  Once DDS_DataReader_delete_contained_entities returns successfully,
   --  the application may delete the DDS_DataReader, knowing that it has no contained
   --- DDS_ReadCondition and DDS_QueryCondition objects.
   --      rasies exception on error

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataReaderQos) is abstract;

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataReaderQos) is abstract;

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DataReaderListener.Ref_Access;
      Mask       : in DDS.StatusMask) is abstract;

   function Get_Listener
     (Self : not null access Ref)
      return DDS.DataReaderListener.Ref_Access is abstract;

   function Get_Topicdescription
     (Self : not null access Ref)
      return DDS.TopicDescription.Ref_Access is abstract;

   function Get_Subscriber
     (Self : not null access Ref)
      return access DDS.Subscriber.Ref'Class is abstract;

   function Get_Sample_Rejected_Status
     (Self : not null access Ref)
      return DDS.SampleRejectedStatus is abstract;

   function Get_Liveliness_Changed_Status
     (Self : not null access Ref)
      return DDS.LivelinessChangedStatus is abstract;

   function Get_Requested_Deadline_Missed_Status
     (Self : not null access Ref)
      return DDS.RequestedDeadlineMissedStatus is abstract;

   function Get_Requested_Incompatible_Qos_Status
     (Self : not null access Ref)
      return DDS.RequestedIncompatibleQosStatus is abstract;

   function Get_Subscription_Match_Status
     (Self : not null access Ref)
      return DDS.SubscriptionMatchedStatus is abstract;

   function Get_Sample_Lost_Status
     (Self : not null access Ref)
      return DDS.SampleLostStatus is abstract;

   function Wait_For_Historical_Data
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
     return DDS.ReturnCode_T is abstract;

   procedure Get_Matched_Publications
     (Self                : not null access Ref;
      Publication_Handles : in out DDS.InstanceHandle_Seq.Sequence;
      Returns             : out DDS.ReturnCode_T) is abstract;

   procedure Get_Matched_Publication_Data
     (Self               : not null access Ref;
      Publication_Data   : in out DDS.PublicationBuiltinTopicData;
      Publication_Handle : in DDS.InstanceHandle_T;
      Returns            : out DDS.ReturnCode_T) is abstract;


end DDS.DataReader;
