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

with DDS.DataWriter;
with DDS.Domain_Entity_Impl;
limited with DDS.Publisher;
with DDS.Topic;
with DDS.DataWriterListener;

with System;


package DDS.DataWriter_Impl is

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.DataWriter.Ref with null record;
   type Ref_Access is access all Ref'Class;

   --
   procedure  Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataWriterQos);

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataWriterQos);

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DataWriterListener.Ref_Access;
      Mask       : in DDS.StatusMask);

   function Get_Listener
     (Self : not null access Ref)
      return DDS.DataWriterListener.Ref_Access;

   function Get_Topic
     (Self : not null access Ref)
      return DDS.Topic.Ref_Access;

   function Get_Publisher
     (Self : not null access Ref)
      return access DDS.Publisher.Ref'Class;

   function Wait_For_Acknowledgments
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
      return DDS.ReturnCode_T;

   function Get_Liveliness_Lost_Status
     (Self : not null access Ref)
      return DDS.LivelinessLostStatus;

   function Get_Offered_Deadline_Missed_Status
     (Self : not null access Ref)
      return DDS.OfferedDeadlineMissedStatus;

   function Get_Offered_Incompatible_Qos_Status
     (Self : not null access Ref)
      return DDS.OfferedIncompatibleQosStatus;

   function Get_Publication_Match_Status
     (Self : not null access Ref)
      return DDS.PublicationMatchedStatus;

   function Assert_Liveliness
     (Self : not null access Ref)
      return DDS.ReturnCode_T;

--     procedure Get_Matched_Subscriptions
--       (Self                 : not null access Ref;
--        Subscription_Handles : in out DDS.InstanceHandleSeq;
--        Returns              : out DDS.ReturnCode_T);

   procedure Get_Matched_Subscription_Data
     (Self                : not null access Ref;
      Subscription_Data   : in DDS.SubscriptionBuiltinTopicData_Access;
      Subscription_Handle : in DDS.InstanceHandle_T;
      Returns             : out DDS.ReturnCode_T);


   procedure Write
     (Self          : not null access Ref;
      Instance_Data : in System.Address;
      Handle        : in DDS.InstanceHandle_T_Access);

   procedure Free (This : in out Ref_Access);



   function CreateI (C_Publisher  : System.Address;
                     A_Topic      : access DDS.Topic.Ref'Class;
                     Qos          : in DDS.DataWriterQos;
                     A_Listener   : in DDS.DataWriterListener.Ref_Access;
                     Mask         : in DDS.StatusMask)
                    return DDS.DataWriter.Ref_Access;

   function Get_FacadeI (C_DataWriter : System.Address)
                        return Ref_Access;


private

   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

end DDS.DataWriter_Impl;
