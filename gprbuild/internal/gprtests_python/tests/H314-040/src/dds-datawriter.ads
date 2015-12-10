pragma Ada_05;
-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

limited with DDS.Publisher;
with DDS.Topic;
with DDS.DataWriterListener;
with DDS.Domain_Entity;

with System;


package DDS.DataWriter is

   type Ref is interface and DDS.Domain_Entity.Ref;
   type Ref_Access is access all Ref'Class;

   procedure  Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataWriterQos) is abstract;

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataWriterQos) is abstract;

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DataWriterListener.Ref_Access;
      Mask       : in DDS.StatusMask) is abstract;

   function Get_Listener
     (Self : not null access Ref)
      return DDS.DataWriterListener.Ref_Access is abstract;

   function Get_Topic
     (Self : not null access Ref)
      return DDS.Topic.Ref_Access is abstract;

   function Get_Publisher
     (Self : not null access Ref)
      return access DDS.Publisher.Ref'Class is abstract;

   function Wait_For_Acknowledgments
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
      return DDS.ReturnCode_T is abstract;

   function Get_Liveliness_Lost_Status
     (Self : not null access Ref)
      return DDS.LivelinessLostStatus is abstract;

   function Get_Offered_Deadline_Missed_Status
     (Self : not null access Ref)
      return DDS.OfferedDeadlineMissedStatus is abstract;

   function Get_Offered_Incompatible_Qos_Status
     (Self : not null access Ref)
      return DDS.OfferedIncompatibleQosStatus is abstract;

   function Get_Publication_Match_Status
     (Self : not null access Ref)
      return DDS.PublicationMatchedStatus is abstract;

   function Assert_Liveliness
     (Self : not null access Ref)
      return DDS.ReturnCode_T is abstract;

--     procedure Get_Matched_Subscriptions
--       (Self                 : not null access Ref;
--        Subscription_Handles : in out DDS.InstanceHandleSeq;
--        Returns              : out DDS.ReturnCode_T);

   procedure Get_Matched_Subscription_Data
     (Self                : not null access Ref;
      Subscription_Data   : in DDS.SubscriptionBuiltinTopicData_Access;
      Subscription_Handle : in DDS.InstanceHandle_T;
      Returns             : out DDS.ReturnCode_T) is abstract;


   procedure Write
     (Self          : not null access Ref;
      Instance_Data : in System.Address;
      Handle        : in DDS.InstanceHandle_T_Access) is abstract;

end DDS.DataWriter;
