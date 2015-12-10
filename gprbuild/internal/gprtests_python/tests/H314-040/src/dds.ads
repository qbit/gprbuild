pragma Ada_05;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Interfaces;
with Interfaces.C.Strings;

with DDS_Support;
with DDS_Support.Sequences_Generic;

with System;

with RTI.Cdr;
with RTI.Osapi_Thread;
--  with RTI.Disc_Dynamic;

package DDS is
--  Note that this package dependes heavely on the underlaying C implementation
--  and will change when a full Ada implementation is avalible.
--
--  Worth to notice is that erros from the underlaying midlleware is
--  translated to exceptions, since there is no posibillity to ignore
--  function returns in Ada.
--
--  From osapi_types.h
--

   type Short              is new Interfaces.Integer_16;
   type Long               is new Interfaces.Integer_32;
   subtype Natural         is Long range 0 .. Long'Last;
   subtype Positive        is Long range 1 .. Long'Last;

   type Long_Long          is new Interfaces.Integer_64;
   type Unsigned_Short     is new Interfaces.Unsigned_16;
   type Unsigned_Long      is new Interfaces.Unsigned_32;
   type Unsigned_Long_Long is new Interfaces.Unsigned_64;
   type Float              is new Interfaces.IEEE_Float_32;
   type Double             is new Interfaces.IEEE_Float_64;
   type Long_Double        is new Interfaces.IEEE_Extended_Float;
   type Char               is new Standard.Character;
   type Wchar              is new Standard.Wide_Character;
   type Octet              is new Interfaces.Unsigned_8;
   subtype Boolean         is Standard.Boolean;
   subtype String          is Interfaces.C.Strings.chars_ptr;
   type Wide_String        is new Standard.Wide_String;

   --  Pointers on the previous types

   type    Short_Ptr              is access all Short;
   type    Long_Ptr               is access all Long;
   type    Long_Long_Ptr          is access all Long_Long;
   type    Unsigned_Short_Ptr     is access all Unsigned_Short;
   type    Unsigned_Long_Ptr      is access all Unsigned_Long;
   type    Unsigned_Long_Long_Ptr is access all Unsigned_Long_Long;
   type    Float_Ptr              is access all Float;
   type    Double_Ptr             is access all Double;
   type    Long_Double_Ptr        is access all Long_Double;
   type    Char_Ptr               is access all Char;
   type    Wchar_Ptr              is access all Wchar;
   type    Octet_Ptr              is access all Octet;
   type    Boolean_Ptr            is access all Boolean;
   type    String_Ptr             is access all String;
   type    Wide_String_Ptr        is access all Wide_String;

   --  ... and deallocation method for each pointer type

   procedure Deallocate is new Ada.Unchecked_Deallocation (Short, Short_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Long, Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Long_Long, Long_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Unsigned_Short, Unsigned_Short_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Unsigned_Long, Unsigned_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unsigned_Long_Long, Unsigned_Long_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Float, Float_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Double, Double_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Long_Double, Long_Double_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Char, Char_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Wchar, Wchar_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Octet, Octet_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Boolean, Boolean_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (String, String_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Ptr);

   function To_DDS_String (Source : Standard.String) return DDS.String
                           renames Interfaces.C.Strings.New_String;

   function To_Standard_String (Source : DDS.String) return Standard.String
                                renames Interfaces.C.Strings.Value;

   NULL_STRING : constant DDS.String := Interfaces.C.Strings.Null_Ptr;

   --
   --  From dds_c/dds_c_infrastructure.h
   --
   type Short_Array is array (Natural range <>) of aliased Short;
   procedure Initialize (Self  : in out Short);
   procedure Finalize (Self  : in out Short);
   procedure Copy (Dst : in out Short; Src : in Short);
   package Short_Seq is new DDS_Support.Sequences_Generic
     (Short,
      Short_Ptr,
      DDS.Natural,
      1,
      Short_Array);

   type Long_Array is array (Natural range <>) of aliased Long;
   procedure Initialize (Self  : in out Long);
   procedure Finalize (Self  : in out Long);
   procedure Copy (Dst : in out Long; Src : in Long);
   package Long_Seq is new DDS_Support.Sequences_Generic
     (Long,
      Long_Ptr,
      DDS.Natural,
      1,
      Long_Array);

   type Long_Long_Array is array (Natural range <>) of aliased Long_Long;
   procedure Initialize (Self  : in out Long_Long);
   procedure Finalize (Self  : in out Long_Long);
   procedure Copy (Dst : in out Long_Long; Src : in Long_Long);
   package Long_Long_Seq is new DDS_Support.Sequences_Generic
     (Long_Long,
      Long_Long_Ptr,
      DDS.Natural,
      1,
      Long_Long_Array);

   type Unsigned_Short_Array is array (Natural range <>) of aliased Unsigned_Short;
   procedure Initialize (Self  : in out Unsigned_Short);
   procedure Finalize (Self  : in out Unsigned_Short);
   procedure Copy (Dst : in out Unsigned_Short; Src : in Unsigned_Short);
   package Unsigned_Short_Seq is new DDS_Support.Sequences_Generic
     (Unsigned_Short,
      Unsigned_Short_Ptr,
      DDS.Natural,
      1,
      Unsigned_Short_Array);

   type Unsigned_Long_Array is array (Natural range <>) of aliased Unsigned_Long;
   procedure Initialize (Self  : in out Unsigned_Long);
   procedure Finalize (Self  : in out Unsigned_Long);
   procedure Copy (Dst : in out Unsigned_Long; Src : in Unsigned_Long);
   package Unsigned_Long_Seq is new DDS_Support.Sequences_Generic
     (Unsigned_Long,
      Unsigned_Long_Ptr,
      DDS.Natural,
      1,
      Unsigned_Long_Array);

   type Unsigned_Long_Long_Array is array (Natural range <>) of aliased Unsigned_Long_Long;
   procedure Initialize (Self  : in out Unsigned_Long_Long);
   procedure Finalize (Self  : in out Unsigned_Long_Long);
   procedure Copy (Dst : in out Unsigned_Long_Long; Src : in Unsigned_Long_Long);
   package Unsigned_Long_Long_Seq is new DDS_Support.Sequences_Generic
     (Unsigned_Long_Long,
      Unsigned_Long_Long_Ptr,
      DDS.Natural,
      1,
      Unsigned_Long_Long_Array);

   type Float_Array is array (Natural range <>) of aliased Float;
   procedure Initialize (Self  : in out Float);
   procedure Finalize (Self  : in out Float);
   procedure Copy (Dst : in out Float; Src : in Float);
   package Float_Seq is new DDS_Support.Sequences_Generic
     (Float,
      Float_Ptr,
      DDS.Natural,
      1,
      Float_Array);

   type Double_Array is array (Natural range <>) of aliased Double;
   procedure Initialize (Self  : in out Double);
   procedure Finalize (Self  : in out Double);
   procedure Copy (Dst : in out Double; Src : in Double);
   package Double_Seq is new DDS_Support.Sequences_Generic
     (Double,
      Double_Ptr,
      DDS.Natural,
      1,
      Double_Array);

   type Long_Double_Array is array (Natural range <>) of aliased Long_Double;
   procedure Initialize (Self  : in out Long_Double);
   procedure Finalize (Self  : in out Long_Double);
   procedure Copy (Dst : in out Long_Double; Src : in Long_Double);
   package Long_Double_Seq is new DDS_Support.Sequences_Generic
     (Long_Double,
      Long_Double_Ptr,
      DDS.Natural,
      1,
      Long_Double_Array);

   type Char_Array is array (Natural range <>) of aliased Char;
   procedure Initialize (Self  : in out Char);
   procedure Finalize (Self  : in out Char);
   procedure Copy (Dst : in out Char; Src : in Char);
   package Char_Seq is new DDS_Support.Sequences_Generic
     (Char,
      Char_Ptr,
      DDS.Natural,
      1,
      Char_Array);

   type Wchar_Array is array (Natural range <>) of aliased Wchar;
   procedure Initialize (Self  : in out Wchar);
   procedure Finalize (Self  : in out Wchar);
   procedure Copy (Dst : in out Wchar; Src : in Wchar);
   package Wchar_Seq is new DDS_Support.Sequences_Generic
     (Wchar,
      Wchar_Ptr,
      DDS.Natural,
      1,
      Wchar_Array);

   type Octet_Array is array (Natural range <>) of aliased Octet;
   procedure Initialize (Self  : in out Octet);
   procedure Finalize (Self  : in out Octet);
   procedure Copy (Dst : in out Octet; Src : in Octet);
   package Octet_Seq is new DDS_Support.Sequences_Generic
     (Octet,
      Octet_Ptr,
      DDS.Natural,
      1,
      Octet_Array);

   type Boolean_Array is array (Natural range <>) of aliased Boolean;
   procedure Initialize (Self  : in out Boolean);
   procedure Finalize (Self  : in out Boolean);
   procedure Copy (Dst : in out Boolean; Src : in Boolean);
   package Boolean_Seq is new DDS_Support.Sequences_Generic
     (Boolean,
      Boolean_Ptr,
      DDS.Natural,
      1,
      Boolean_Array);


   type String_Array is array (Natural range <>) of aliased DDS.String;
   procedure Initialize (Self  : in out DDS.String);
   procedure Finalize (Self  : in out DDS.String);
   procedure Copy (Dst : in out DDS.String; Src : in DDS.String);
   package String_Seq is new DDS_Support.Sequences_Generic
     (DDS.String,
      String_Ptr,
      DDS.Natural,
      1,
      String_Array);

   --     type Wide_String_Array is array (Natural range <>) of aliased Wide_String;
   --     procedure Initialize (Self  : in out Wide_String);
   --     procedure Finalize (Self  : in out Wide_String);
   --     procedure Copy (Dst : in out Wide_String; Src : in Wide_String);
   --     package Wide_String_Seq is new DDS_Support.Sequences_Generic
   --       (Wide_String,
   --        Wide_String_Ptr,
   --        DDS.Natural,
   --        1,
   --        Wide_String_Array);



   type Time_T is record
      Sec     : Long;
      Nanosec : Unsigned_Long;
   end record;
   pragma Convention (C, Time_T);

   function "<" (L : Time_T; R : Time_T) return Boolean;

   function ">" (L : Time_T; R : Time_T) return Boolean;

   function "<=" (L : Time_T; R : Time_T) return Boolean;

   function ">=" (L : Time_T; R : Time_T) return Boolean;

   function "+" (L : Time_T; R : Time_T) return Time_T;

   Time_Zero : constant Time_T := (0, 0);
   TIME_INVALID_SEC : constant Long      := -1;
   TIME_INVALID_NSEC : constant Unsigned_Long      := 4_294_967_295;
   Time_Invalid : constant Time_T := (TIME_INVALID_SEC, TIME_INVALID_NSEC);

   function Time_Is_Zero (T : Time_T) return Boolean;

   function Time_Is_Invalid (T : Time_T) return Boolean;

   type Duration_T is record
      Sec     : Long;
      Nanosec : Unsigned_Long;
   end record;
   pragma Convention (C, Duration_T);

   DURATION_ZERO_SEC : constant Long := 0;
   DURATION_ZERO_NSEC : constant Unsigned_Long := 0;
   DURATION_ZERO : Duration_T := (DURATION_ZERO_SEC, DURATION_ZERO_NSEC);
   DURATION_INFINITE_SEC : constant Long := 2_147_483_647;
   DURATION_INFINITE_NSEC : constant Unsigned_Long := 2_1474_83_647;
   DURATION_INFINITE : constant Duration_T :=
                         (DURATION_INFINITE_SEC, DURATION_INFINITE_NSEC);

   function Duration_Is_Zero (D : Duration_T) return Boolean;

   function Duration_Is_Infinite (D : Duration_T) return Boolean;


   type Builtin_Topic_Key_Type_Native is new Interfaces.Unsigned_32;

   subtype InstanceHandle_T is DDS_Support.InstanceHandle_T;

   Null_InstanceHandle_T : constant InstanceHandle_T := DDS_Support.Null_InstanceHandle_T;
   HANDLE_NIL : constant InstanceHandle_T := Null_InstanceHandle_T;

   type InstanceHandle_T_Access is access all InstanceHandle_T;

   type InstanceHandle_T_Array is array (Natural range <>) of aliased InstanceHandle_T;
   procedure Initialize (Self  : in out InstanceHandle_T);
   procedure Finalize (Self  : in out InstanceHandle_T);
   procedure Copy (Dst : in out InstanceHandle_T; Src : in InstanceHandle_T);
   package InstanceHandle_Seq is new DDS_Support.Sequences_Generic
     (InstanceHandle_T,
      InstanceHandle_T_Access,
      DDS.Natural,
      1,
      InstanceHandle_T_Array);

   type Guid_T is record
      Value : aliased Octet_Array (0 .. 15);
   end record;
   pragma Convention (C, Guid_T);
   type Guid_T_Access is access all Guid_T;


   procedure Guid_Copy (Self  : Guid_T_Access;
                        Other : Guid_T_Access);
   pragma Interface (C, Guid_Copy, "DDS_GUID_copy");

   procedure Guid_Print (Self   : Guid_T_Access;
                         Desc   : String;
                         Indent : Unsigned_Long);
   pragma Interface (C, Guid_Print, "DDS_GUID_print");

   procedure Guid_Zero (Self   : Guid_T_Access);
   pragma Interface (C, Guid_Zero, "DDS_GUID_xero");

   type SequenceNumber_T is record
      High : Long;
      Low  : Unsigned_Long;
   end record;
   pragma Convention (C, SequenceNumber_T);
   type SequenceNumber_T_Access is access all  SequenceNumber_T;

   SEQUENCE_NUMBER_UNKNOWN : constant  SequenceNumber_T;
   pragma Interface (C, SEQUENCE_NUMBER_UNKNOWN, "DDS_SEQUENCE_NUMBER_UNKNOWN");

   SEQUENCE_NUMBER_ZERO : constant  SequenceNumber_T;
   pragma Interface (C, SEQUENCE_NUMBER_ZERO, "DDS_SEQUENCE_NUMBER_ZERO");

   SEQUENCE_NUMBER_MAX : constant  SequenceNumber_T;
   pragma Interface (C, SEQUENCE_NUMBER_MAX, "DDS_SEQUENCE_NUMBER_MAX");

   function SequenceNumber_Compare (Sn1 : SequenceNumber_T_Access;
                                    Sn2 : SequenceNumber_T_Access)
                                    return Long;

   type OriginalWriterInfo_T is record
      Writer_Guid     : Guid_T;
      Sequence_Number : SequenceNumber_T;
   end record;
   pragma Convention (C, OriginalWriterInfo_T);


   ERROR                : exception;
   UNSUPPORTED          : exception;
   BAD_PARAMETER        : exception;
   PRECONDITION_NOT_MET : exception;
   OUT_OF_RESOURCES     : exception;
   NOT_ENABLED          : exception;
   IMMUTABLE_POLICY     : exception;
   INCONSISTENT_POLICY  : exception;
   ALREADY_DELETED      : exception;
   TIMEOUT              : exception;
   NO_DATA              : exception;
   DDS_ERROR            : exception;

   type ReturnCode_T is
     (RETCODE_OK,
      --  Successful return.

      RETCODE_ERROR,
      --  Generic, unspecified error.

      RETCODE_UNSUPPORTED,
      --  Unsupported operation. Can only returned by operations that are unsupported.
      RETCODE_BAD_PARAMETER,
      RETCODE_PRECONDITION_NOT_MET,
      RETCODE_OUT_OF_RESOURCES,
      RETCODE_NOT_ENABLED,
      RETCODE_IMMUTABLE_POLICY,
      RETCODE_INCONSISTENT_POLICY,
      RETCODE_ALREADY_DELETED,
      RETCODE_TIMEOUT,
      RETCODE_NO_DATA
     );
   pragma Convention (C, ReturnCode_T);

   procedure Ret_Code_To_Exception (Code : ReturnCode_T; Message : Standard.String := "");

   type StatusKind is new Unsigned_Long;
   INCONSISTENT_TOPIC_STATUS         : constant StatusKind := 2#0000_0000_0000_0001#;
   OFFERED_DEADLINE_MISSED_STATUS    : constant StatusKind := 2#0000_0000_0000_0010#;
   REQUESTED_DEADLINE_MISSED_STATUS  : constant StatusKind := 2#0000_0000_0000_0100#;
   OFFERED_INCOMPATIBLE_QOS_STATUS   : constant StatusKind := 2#0000_0000_0010_0000#;
   REQUESTED_INCOMPATIBLE_QOS_STATUS : constant StatusKind := 2#0000_0000_0100_0000#;
   SAMPLE_LOST_STATUS                : constant StatusKind := 2#0000_0000_1000_0000#;
   SAMPLE_REJECTED_STATUS            : constant StatusKind := 2#0000_0001_0000_0000#;
   DATA_ON_READERS_STATUS            : constant StatusKind := 2#0000_0010_0000_0000#;
   DATA_AVAILABLE_STATUS             : constant StatusKind := 2#0000_0100_0000_0000#;
   LIVELINESS_LOST_STATUS            : constant StatusKind := 2#0000_1000_0000_0000#;
   LIVELINESS_CHANGED_STATUS         : constant StatusKind := 2#0001_0000_0000_0000#;
   PUBLICATION_MATCH_STATUS          : constant StatusKind := 2#0010_0000_0000_0000#;
   SUBSCRIPTION_MATCH_STATUS         : constant StatusKind := 2#0100_0000_0000_0000#;
   --   /* --- Begin extended statuses --- */
   --  /* The "right"-most 24 bits of the StatusMask are reserved
   --   * for standard statuses. The remaining 8 bits are for extended statuses.
   --   */
   RELIABLE_WRITER_CACHE_CHANGED_STATUS     : constant StatusKind :=
                                                2#0001_0000_0000_0000_0000_0000_0000#;
   RELIABLE_READER_ACTIVITY_CHANGED_STATUS  : constant StatusKind :=
                                                2#0010_0000_0000_0000_0000_0000_0000#;
   WRITER_MATCHED_STATISTICS_REQUESTED_STATUS : constant StatusKind :=
                                                  2#0100_0000_0000_0000_0000_0000_0000#;
   READER_MATCHED_STATISTICS_REQUESTED_STATUS : constant StatusKind :=
                                                  2#1000_0000_0000_0000_0000_0000_0000#;

   subtype StatusMask is StatusKind;
   STATUS_MASK_NONE : constant StatusMask := 2#0000_0000_0000_0000#;
   STATUS_MASK_ALL  : constant StatusMask := 2#1111_1111_1111_1111#;

   type StatusKind_Access is access constant StatusKind;

   type ThreadSettings is new Unsigned_Long;
   subtype ThreadSettingsMask is ThreadSettings;
   THREAD_SETTINGS_OPTION_DEFAULT             :
   constant := RTI.Osapi_Thread.OPTION_DEFAULT;
   THREAD_SETTINGS_OPTION_FLOATING_POINT      :
   constant := RTI.Osapi_Thread.OPTION_FLOATING_POINT;
   THREAD_SETTINGS_OPTION_STDIO               :
   constant := RTI.Osapi_Thread.OPTION_STDIO;
   THREAD_SETTINGS_OPTION_REALTIME_PRIORITY   :
   constant := RTI.Osapi_Thread.OPTION_REALTIME_PRIORITY;
   THREAD_SETTINGS_OPTION_PRIORITY_ENFORCE    :
   constant := RTI.Osapi_Thread.OPTION_PRIORITY_ENFORCE;
   THREAD_SETTINGS_OPTION_CANCEL_ASYNCHRONOUS :
   constant := RTI.Osapi_Thread.OPTION_CANCEL_ASYNCHRONOUS;

   type ThreadSettings_T is record
      Mask       : ThreadSettingsMask;
      Priority   : Long;
      Stack_Size : Long;
   end record;
   pragma Convention (C, ThreadSettings_T);

   THREAD_SETTINGS_DEFAULT : constant  ThreadSettings_T :=
                               (Mask       => THREAD_SETTINGS_OPTION_DEFAULT,
                                Priority   => Long (RTI.Osapi_Thread.PRIORITY_DEFAULT),
                                Stack_Size => Long (RTI.Osapi_Thread.STACK_SIZE_DEFAULT));

   type QosPolicyId_T is
     (
      INVALID_QOS_POLICY_ID,
      USERDATA_QOS_POLICY_ID,
      DURABILITY_QOS_POLICY_ID,
      PRESENTATION_QOS_POLICY_ID,
      DEADLINE_QOS_POLICY_ID,
      LATENCYBUDGET_QOS_POLICY_ID,
      OWNERSHIP_QOS_POLICY_ID,
      OWNERSHIPSTRENGTH_QOS_POLICY_ID,
      LIVELINESS_QOS_POLICY_ID,
      TIMEBASEDFILTER_QOS_POLICY_ID,
      PARTITION_QOS_POLICY_ID,
      RELIABILITY_QOS_POLICY_ID,
      DESTINATIONORDER_QOS_POLICY_ID,
      HISTORY_QOS_POLICY_ID,
      RESOURCELIMITS_QOS_POLICY_ID,
      ENTITYFACTORY_QOS_POLICY_ID,
      WRITERDATALIFECYCLE_QOS_POLICY_ID,
      READERDATALIFECYCLE_QOS_POLICY_ID,
      TOPICDATA_QOS_POLICY_ID,
      GROUPDATA_QOS_POLICY_ID,
      TRANSPORTPRIORITY_QOS_POLICY_ID,
      LIFESPAN_QOS_POLICY_ID,
      DURABILITYSERVICE_QOS_POLICY_ID,
      --    /* --- Extension QoS policies: --- */
      --/*i
      --  * We start at 1000 to allow room for future policies added to
      --  * the \dds specification.
      --  */
      --  /*e \dref_QosPolicyId_t_WIREPROTOCOL_QOS_POLICY_ID
      --  */
      WIREPROTOCOL_QOS_POLICY_ID,
      DISCOVERY_QOS_POLICY_ID,
      DATAREADERRESOURCELIMITS_QOS_POLICY_ID,
      DATAWRITERRESOURCELIMITS_QOS_POLICY_ID,
      DATAREADERPROTOCOL_QOS_POLICY_ID,
      DATAWRITERPROTOCOL_QOS_POLICY_ID,
      DOMAINPARTICIPANTRESOURCELIMITS_QOS_POLICY_ID,
      EVENT_QOS_POLICY_ID,
      DATABASE_QOS_POLICY_ID,
      RECEIVERPOOL_QOS_POLICY_ID,
      DISCOVERYCONFIG_QOS_POLICY_ID,
      EXCLUSIVEAREA_QOS_POLICY_ID,
      USEROBJECT_QOS_POLICY_ID,
      SYSTEMRESOURCELIMITS_QOS_POLICY_ID,
      TRANSPORTSELECTION_QOS_POLICY_ID,
      TRANSPORTUNICAST_QOS_POLICY_ID,
      TRANSPORTMULTICAST_QOS_POLICY_ID,
      TRANSPORTBUILTIN_QOS_POLICY_ID,
      TYPESUPPORT_QOS_POLICY_ID,
      PROPERTY_QOS_POLICY_ID,
      PUBLISHMODE_QOS_POLICY_ID,
      ASYNCHRONOUSPUBLISHER_QOS_POLICY_ID,
      ENTITYNAME_QOS_POLICY_ID,
      STATISTICS_QOS_POLICY_ID,
      SERVICE_QOS_POLICY_ID
     );

   pragma Convention (C, QosPolicyId_T);

   for QosPolicyId_T use
     (INVALID_QOS_POLICY_ID                         => 0,
      USERDATA_QOS_POLICY_ID                        => 1,
      DURABILITY_QOS_POLICY_ID                      => 2,
      PRESENTATION_QOS_POLICY_ID                    => 3,
      DEADLINE_QOS_POLICY_ID                        => 4,
      LATENCYBUDGET_QOS_POLICY_ID                   => 5,
      OWNERSHIP_QOS_POLICY_ID                       => 6,
      OWNERSHIPSTRENGTH_QOS_POLICY_ID               => 7,
      LIVELINESS_QOS_POLICY_ID                      => 8,
      TIMEBASEDFILTER_QOS_POLICY_ID                 => 9,
      PARTITION_QOS_POLICY_ID                       => 10,
      RELIABILITY_QOS_POLICY_ID                     => 11,
      DESTINATIONORDER_QOS_POLICY_ID                => 12,
      HISTORY_QOS_POLICY_ID                         => 13,
      RESOURCELIMITS_QOS_POLICY_ID                  => 14,
      ENTITYFACTORY_QOS_POLICY_ID                   => 15,
      WRITERDATALIFECYCLE_QOS_POLICY_ID             => 16,
      READERDATALIFECYCLE_QOS_POLICY_ID             => 17,
      TOPICDATA_QOS_POLICY_ID                       => 18,
      GROUPDATA_QOS_POLICY_ID                       => 19,
      TRANSPORTPRIORITY_QOS_POLICY_ID               => 20,
      LIFESPAN_QOS_POLICY_ID                        => 21,
      DURABILITYSERVICE_QOS_POLICY_ID               => 22,
      WIREPROTOCOL_QOS_POLICY_ID                    => 1000,
      DISCOVERY_QOS_POLICY_ID                       => 1001,
      DATAREADERRESOURCELIMITS_QOS_POLICY_ID        => 1003,
      DATAWRITERRESOURCELIMITS_QOS_POLICY_ID        => 1004,
      DATAREADERPROTOCOL_QOS_POLICY_ID              => 1005,
      DATAWRITERPROTOCOL_QOS_POLICY_ID              => 1006,
      DOMAINPARTICIPANTRESOURCELIMITS_QOS_POLICY_ID => 1007,
      EVENT_QOS_POLICY_ID                           => 1008,
      DATABASE_QOS_POLICY_ID                        => 1009,
      RECEIVERPOOL_QOS_POLICY_ID                    => 1010,
      DISCOVERYCONFIG_QOS_POLICY_ID                 => 1011,
      EXCLUSIVEAREA_QOS_POLICY_ID                   => 1012,
      USEROBJECT_QOS_POLICY_ID                      => 1013,
      SYSTEMRESOURCELIMITS_QOS_POLICY_ID            => 1014,
      TRANSPORTSELECTION_QOS_POLICY_ID              => 1015,
      TRANSPORTUNICAST_QOS_POLICY_ID                => 1016,
      TRANSPORTMULTICAST_QOS_POLICY_ID              => 1017,
      TRANSPORTBUILTIN_QOS_POLICY_ID                => 1018,
      TYPESUPPORT_QOS_POLICY_ID                     => 1019,
      PROPERTY_QOS_POLICY_ID                        => 1020,
      PUBLISHMODE_QOS_POLICY_ID                     => 1021,
      ASYNCHRONOUSPUBLISHER_QOS_POLICY_ID           => 1022,
      ENTITYNAME_QOS_POLICY_ID                      => 1023,
      STATISTICS_QOS_POLICY_ID                      => 1024,
      SERVICE_QOS_POLICY_ID                         => 1025
     );

   type QosPolicyCount is record
      Policy_Id : aliased QosPolicyId_T;
      Count     : aliased Long;
   end record;
   pragma Convention (C, QosPolicyCount);

   type QosPolicyCount_Access is access all QosPolicyCount;

   type QosPolicyCount_Array is array (Natural range <>) of aliased QosPolicyCount;
   procedure Initialize (Self  : in out QosPolicyCount);
   procedure Finalize (Self  : in out QosPolicyCount);
   procedure Copy (Dst : in out QosPolicyCount; Src : in QosPolicyCount);
   package QosPolicyCount_Seq is new DDS_Support.Sequences_Generic
     (QosPolicyCount,
      QosPolicyCount_Access,
      DDS.Natural,
      1,
      QosPolicyCount_Array);

   type EntityKind_T is
     (UNKNOWN_ENTITY_KIND,
      PARTICIPANT_ENTITY_KIND,
      PUBLISHER_ENTITY_KIND,
      SUBSCRIBER_ENTITY_KIND,
      TOPIC_ENTITY_KIND,
      DATAREADER_ENTITY_KIND,
      DATAWRITER_ENTITY_KIND);
   pragma Convention (C, EntityKind_T);

   for EntityKind_T use
     (UNKNOWN_ENTITY_KIND     => 0,
      PARTICIPANT_ENTITY_KIND => 1,
      PUBLISHER_ENTITY_KIND   => 2,
      SUBSCRIBER_ENTITY_KIND  => 3,
      TOPIC_ENTITY_KIND       => 4,
      DATAREADER_ENTITY_KIND  => 5,
      DATAWRITER_ENTITY_KIND  => 6);

   USERDATA_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, USERDATA_QOS_POLICY_NAME, "DDS_USERDATA_QOS_POLICY_NAME");

   type UserDataQosPolicy is record
      Value : aliased Octet_Seq.Sequence;
   end record;
   pragma Convention (C, UserDataQosPolicy);

   USER_DATA_QOS_POLICY_DEFAULT : constant UserDataQosPolicy :=
                                    (Value => Octet_Seq.DEFAULT_SEQUENCE);

   TOPICDATA_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TOPICDATA_QOS_POLICY_NAME, "DDS_TOPICDATA_QOS_POLICY_NAME");

   type TopicDataQosPolicy is record
      Value : aliased Octet_Seq.Sequence;
   end record;
   pragma Convention (C, TopicDataQosPolicy);

   TOPIC_DATA_QOS_POLICY_DEFAULT : constant TopicDataQosPolicy :=
                                     (Value => Octet_Seq.DEFAULT_SEQUENCE);

   GROUPDATA_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, GROUPDATA_QOS_POLICY_NAME, "DDS_GROUPDATA_QOS_POLICY_NAME");

   type GroupDataQosPolicy is record
      Value : aliased Octet_Seq.Sequence;
   end record;
   pragma Convention (C, GroupDataQosPolicy);

   GROUP_DATA_QOS_POLICY_DEFAULT : constant GroupDataQosPolicy :=
                                     (Value => Octet_Seq.DEFAULT_SEQUENCE);

   DURABILITY_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DURABILITY_QOS_POLICY_NAME, "DDS_DURABILITY_QOS_POLICY_NAME");

   type DurabilityQosPolicyKind is
     (VOLATILE_DURABILITY_QOS,
      TRANSIENT_LOCAL_DURABILITY_QOS,
      TRANSIENT_DURABILITY_QOS,
      PERSISTENT_DURABILITY_QOS);
   pragma Convention (C, DurabilityQosPolicyKind);

   type DurabilityQosPolicy is record
      Kind                 : aliased DurabilityQosPolicyKind;
      Direct_Communication : aliased DDS.Boolean;
      Spare1               : aliased DDS.Boolean;
      Spare2               : aliased DDS.Boolean;
      Spare3               : aliased DDS.Boolean;
   end record;
   pragma Convention (C, DurabilityQosPolicy);

   DURABILITY_QOS_POLICY_DEFAULT : constant DurabilityQosPolicy :=
                                     (Kind                 => VOLATILE_DURABILITY_QOS,
                                      Direct_Communication => True,
                                      Spare1               => False,
                                      Spare2               => False,
                                      Spare3               => False);

   PRESENTATION_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, PRESENTATION_QOS_POLICY_NAME, "DDS_PRESENTATION_QOS_POLICY_NAME");

   type PresentationQosPolicyAccessScopeKind is
     (INSTANCE_PRESENTATION_QOS,
      TOPIC_PRESENTATION_QOS,
      GROUP_PRESENTATION_QOS);
   pragma Convention (C, PresentationQosPolicyAccessScopeKind);

   type PresentationQosPolicy  is record
      Access_Scope    : aliased PresentationQosPolicyAccessScopeKind;
      Coherent_Access : aliased DDS.Boolean;
      Ordered_Access  : aliased DDS.Boolean;
      Spare1          : aliased DDS.Boolean;
      Spare2          : aliased DDS.Boolean;
   end record;
   pragma Convention (C, PresentationQosPolicy);

   PRESENTATION_QOS_POLICY_DEFAULT : constant PresentationQosPolicy :=
                                       (INSTANCE_PRESENTATION_QOS,
                                        FALSE,
                                        FALSE,
                                        FALSE,
                                        FALSE);

   DEADLINE_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DEADLINE_QOS_POLICY_NAME, "DDS_DEADLINE_QOS_POLICY_NAME");

   type DeadlineQosPolicy is record
      Period : aliased Duration_T;
   end record;
   pragma Convention (C, DeadlineQosPolicy);

   DEADLINE_QOS_POLICY_DEFAULT : constant DeadlineQosPolicy :=
                                   (Period => DURATION_INFINITE);

   LATENCYBUDGET_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, LATENCYBUDGET_QOS_POLICY_NAME, "DDS_LATENCYBUDGET_QOS_POLICY_NAME");

   type LatencyBudgetQosPolicy is record
      Duration : aliased Duration_T;
   end record;
   pragma Convention (C, LatencyBudgetQosPolicy);

   LATENCY_BUDGET_QOS_POLICY_DEFAULT : constant LatencyBudgetQosPolicy :=
                                         (Duration => DURATION_ZERO);

   OWNERSHIP_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, OWNERSHIP_QOS_POLICY_NAME, "DDS_OWNERSHIP_QOS_POLICY_NAME");

   type OwnershipQosPolicyKind is
     (SHARED_OWNERSHIP_QOS,
      EXCLUSIVE_OWNERSHIP_QOS);
   pragma Convention (C, OwnershipQosPolicyKind);

   type OwnershipQosPolicy is record
      Kind : aliased OwnershipQosPolicyKind;
   end record;
   pragma Convention (C, OwnershipQosPolicy);

   OWNERSHIP_QOS_POLICY_DEFAULT : constant OwnershipQosPolicy :=
                                    (Kind => SHARED_OWNERSHIP_QOS);

   OWNERSHIPSTRENGTH_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, OWNERSHIPSTRENGTH_QOS_POLICY_NAME,
                     "DDS_OWNERSHIPSTRENGTH_QOS_POLICY_NAME");

   type OwnershipStrengthQosPolicy is record
      Value : aliased Long;
   end record;
   pragma Convention (C, OwnershipStrengthQosPolicy);

   OWNERSHIP_STRENGTH_QOS_POLICY_DEFAULT : constant OwnershipStrengthQosPolicy :=
                                             (Value => 0);

   LIVELINESS_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, LIVELINESS_QOS_POLICY_NAME, "DDS_LIVELINESS_QOS_POLICY_NAME");

   type LivelinessQosPolicyKind is
     (AUTOMATIC_LIVELINESS_QOS,
      MANUAL_BY_PARTICIPANT_LIVELINESS_QOS,
      MANUAL_BY_TOPIC_LIVELINESS_QOS);
   pragma Convention (C, LivelinessQosPolicyKind);

   type LivelinessQosPolicy is record
      Kind           : aliased LivelinessQosPolicyKind;
      Lease_Duration : aliased Duration_T;
   end record;
   pragma Convention (C, LivelinessQosPolicy);

   LIVELINESS_QOS_POLICY_DEFAULT : constant LivelinessQosPolicy :=
                                     (AUTOMATIC_LIVELINESS_QOS,
                                      DURATION_INFINITE);

   TIMEBASEDFILTER_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TIMEBASEDFILTER_QOS_POLICY_NAME,
                     "DDS_TIMEBASEDFILTER_QOS_POLICY_NAME");

   type TimeBasedFilterQosPolicy is record
      Minimum_Separation : aliased Duration_T;
   end record;
   pragma Convention (C, TimeBasedFilterQosPolicy);
   TIME_BASED_FILTER_QOS_POLICY_DEFAULT : constant  TimeBasedFilterQosPolicy :=
                                            (Minimum_Separation => DURATION_ZERO);

   PARTITION_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, PARTITION_QOS_POLICY_NAME, "DDS_PARTITION_QOS_POLICY_NAME");

   type PartitionQosPolicy is record
      Name : aliased String_Seq.Sequence;
   end record;
   pragma Convention (C, PartitionQosPolicy);

   PARTITION_QOS_POLICY_DEFAULT : constant PartitionQosPolicy :=
                                    (Name => String_Seq.DEFAULT_SEQUENCE);

   RELIABILITY_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, RELIABILITY_QOS_POLICY_NAME, "DDS_RELIABILITY_QOS_POLICY_NAME");

   type ReliabilityQosPolicyKind is
     (BEST_EFFORT_RELIABILITY_QOS,
      RELIABLE_RELIABILITY_QOS);
   pragma Convention (C, ReliabilityQosPolicyKind);

   type ReliabilityQosPolicy is record
      Kind              : aliased ReliabilityQosPolicyKind;
      Max_Blocking_Time : aliased Duration_T;
   end record;
   pragma Convention (C, ReliabilityQosPolicy);

   RELIABILITY_QOS_POLICY_DEFAULT : constant ReliabilityQosPolicy :=
                                      (BEST_EFFORT_RELIABILITY_QOS, (0, 100_000_000));

   DESTINATIONORDER_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DESTINATIONORDER_QOS_POLICY_NAME,
                     "DDS_DESTINATIONORDER_QOS_POLICY_NAME");

   type DestinationOrderQosPolicyKind is
     (BY_RECEPTION_TIMESTAMP_DESTINATIONORDER_QOS,
      BY_SOURCE_TIMESTAMP_DESTINATIONORDER_QOS);
   pragma Convention (C, DestinationOrderQosPolicyKind);

   type DestinationOrderQosPolicyScopeKind is
     (INSTANCE_SCOPE_DESTINATIONORDER_QOS,
      TOPIC_SCOPE_DESTINATIONORDER_QOS);
   pragma Convention (C, DestinationOrderQosPolicyScopeKind);

   type DestinationOrderQosPolicy is record
      Kind  : aliased DestinationOrderQosPolicyKind;
      Scope : aliased DestinationOrderQosPolicyScopeKind;
   end record;
   pragma Convention (C, DestinationOrderQosPolicy);

   DESTINATION_ORDER_QOS_POLICY_DEFAULT : constant DestinationOrderQosPolicy :=
                                            (BY_RECEPTION_TIMESTAMP_DESTINATIONORDER_QOS,
                                             INSTANCE_SCOPE_DESTINATIONORDER_QOS);

   HISTORY_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, HISTORY_QOS_POLICY_NAME,
                     "DDS_HISTORY_QOS_POLICY_NAME");

   type HistoryQosPolicyKind is
     (KEEP_LAST_HISTORY_QOS,
      KEEP_ALL_HISTORY_QOS);
   pragma Convention (C, HistoryQosPolicyKind);

   type RefilterQosPolicyKind is
     (NONE_REFILTER_QOS,
      ALL_REFILTER_QOS,
      ON_DEMAND_REFILTER_QOS);
   pragma Convention (C, RefilterQosPolicyKind);

   type HistoryQosPolicy is record
      Kind     : aliased HistoryQosPolicyKind;
      Depth    : aliased Long;
      Refilter : aliased RefilterQosPolicyKind;
   end record;
   pragma Convention (C, HistoryQosPolicy);

   HISTORY_QOS_POLICY_DEFAULT : constant HistoryQosPolicy :=
                                  (KEEP_LAST_HISTORY_QOS,
                                   1,
                                   NONE_REFILTER_QOS);

   LENGTH_UNLIMITED : constant Long  := -1;

   DURABILITYSERVICE_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DURABILITYSERVICE_QOS_POLICY_NAME,
                     "DDS_DURABILITYSERVICE_QOS_POLICY_NAME");

   type DurabilityServiceQosPolicy is record
      Service_Cleanup_Delay    : aliased Duration_T;
      History_Kind             : aliased HistoryQosPolicyKind;
      History_Depth            : aliased Long;
      Max_Samples              : aliased Long;
      Max_Instances            : aliased Long;
      Max_Samples_Per_Instance : aliased Long;
   end record;
   pragma Convention (C, DurabilityServiceQosPolicy);

   DURABILITY_SERVICE_QOS_POLICY_DEFAULT : constant DurabilityServiceQosPolicy :=
                                             (DURATION_ZERO,
                                              KEEP_LAST_HISTORY_QOS,
                                              1,
                                              LENGTH_UNLIMITED,
                                              LENGTH_UNLIMITED,
                                              LENGTH_UNLIMITED);

   RESOURCELIMITS_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, RESOURCELIMITS_QOS_POLICY_NAME,
                     "DDS_RESOURCELIMITS_QOS_POLICY_NAME");

   type ResourceLimitsQosPolicy is record
      Max_Samples              : aliased Long;
      Max_Instances            : aliased Long;
      Max_Samples_Per_Instance : aliased Long;
      Initial_Samples          : aliased Long;
      Initial_Instances        : aliased Long;
   end record;
   pragma Convention (C, ResourceLimitsQosPolicy);

   RESOURCE_LIMITS_QOS_POLICY_DEFAULT : constant ResourceLimitsQosPolicy :=
                                          (LENGTH_UNLIMITED,
                                           LENGTH_UNLIMITED,
                                           LENGTH_UNLIMITED,
                                           32,
                                           32);

   TRANSPORTPRIORITY_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TRANSPORTPRIORITY_QOS_POLICY_NAME,
                     "DDS_TRANSPORTPRIORITY_QOS_POLICY_NAME");

   type TransportPriorityQosPolicy is record
      Value : aliased Long;
   end record;
   pragma Convention (C, TransportPriorityQosPolicy);

   TRANSPORT_PRIORITY_QOS_POLICY_DEFAULT : constant TransportPriorityQosPolicy :=
                                             (Value => 0);

   LIFESPAN_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, LIFESPAN_QOS_POLICY_NAME, "DDS_LIFESPAN_QOS_POLICY_NAME");

   type LifespanQosPolicy is record
      Duration : aliased Duration_T;
   end record;
   pragma Convention (C, LifespanQosPolicy);

   LIFESPAN_QOS_POLICY_DEFAULT : constant LifespanQosPolicy :=
                                   (Duration => DURATION_INFINITE);

   WRITERDATALIFECYCLE_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, WRITERDATALIFECYCLE_QOS_POLICY_NAME,
                     "DDS_WRITERDATALIFECYCLE_QOS_POLICY_NAME");

   type WriterDataLifecycleQosPolicy is record
      Autodispose_Unregistered_Instances : aliased DDS.Boolean;
      Spare1                             : aliased DDS.Boolean;
      Spare2                             : aliased DDS.Boolean;
      Spare3                             : aliased DDS.Boolean;
   end record;
   pragma Convention (C, WriterDataLifecycleQosPolicy);

   WRITER_DATA_LIFECYCLE_QOS_POLICY_DEFAULT : constant WriterDataLifecycleQosPolicy :=
                                                (Autodispose_Unregistered_Instances => True,
                                                 Spare1                             => False,
                                                 Spare2                             => False,
                                                 Spare3                             => False);

   READERDATALIFECYCLE_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, READERDATALIFECYCLE_QOS_POLICY_NAME,
                     "DDS_READERDATALIFECYCLE_QOS_POLICY_NAME");

   type ReaderDataLifecycleQosPolicy is record
      Autopurge_Nowriter_Samples_Delay : aliased Duration_T;
      Autopurge_Disposed_Samples_Delay : aliased Duration_T;
   end record;
   pragma Convention (C, ReaderDataLifecycleQosPolicy);

   READER_DATA_LIFECYCLE_QOS_POLICY_DEFAULT : constant ReaderDataLifecycleQosPolicy :=
                                                (DURATION_INFINITE,
                                                 DURATION_INFINITE);

   ENTITYFACTORY_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, ENTITYFACTORY_QOS_POLICY_NAME,
                     "DDS_ENTITYFACTORY_QOS_POLICY_NAME");

   type EntityFactoryQosPolicy is record
      Autoenable_Created_Entities : aliased DDS.Boolean;
      Spare1                      : aliased DDS.Boolean;
      Spare2                      : aliased DDS.Boolean;
      Spare3                      : aliased DDS.Boolean;
   end record;
   pragma Convention (C, EntityFactoryQosPolicy);

   ENTITY_FACTORY_QOS_POLICY_DEFAULT : constant EntityFactoryQosPolicy :=
                                         (Autoenable_Created_Entities => True,
                                          Spare1                      => False,
                                          Spare2                      => False,
                                          Spare3                      => False);

   --!!!
   --!!! RTI DDS Extensions
   --!!!

   type AllocationSettings_T is record
      Initial_Count     : aliased Long;
      Max_Count         : aliased Long;
      Incremental_Count : aliased Long;
   end record;
   pragma Convention (C, AllocationSettings_T);
   type  AllocationSettings_T_Access is access all  AllocationSettings_T;

   procedure AllocationSettings_Add (Answer : AllocationSettings_T_Access;
                                     Left   : AllocationSettings_T_Access;
                                     Right  : AllocationSettings_T_Access);
   pragma Interface (C, AllocationSettings_Add, "DDS_AllocationSettings_add");

   type RtpsReliableReaderProtocol_T is record
      Min_Heartbeat_Response_Delay   : aliased Duration_T;
      Max_Heartbeat_Response_Delay   : aliased Duration_T;
      Heartbeat_Suppression_Duration : aliased Duration_T;
   end record;
   pragma Convention (C, RtpsReliableReaderProtocol_T);

   RTPS_RELIABLE_READER_PROTOCOL_DEFAULT : constant RtpsReliableReaderProtocol_T :=
                                             (Min_Heartbeat_Response_Delay   => (0, 0),
                                              Max_Heartbeat_Response_Delay   => (0, 500_000_000),
                                              Heartbeat_Suppression_Duration => (0, 62_500_000));

   RTPS_RELIABLE_READER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT :
   constant RtpsReliableReaderProtocol_T :=
                                                              (Min_Heartbeat_Response_Delay   => (0, 0),
                                                               Max_Heartbeat_Response_Delay   => (0, 0),
                                                               Heartbeat_Suppression_Duration => (0, 62_500_000));

   type RtpsReliableWriterProtocol_T is record
      Low_Watermark                : aliased Long;
      High_Watermark               : aliased Long;
      Heartbeat_Period             : aliased Duration_T;
      Fast_Heartbeat_Period        : aliased Duration_T;
      Late_Joiner_Heartbeat_Period : aliased Duration_T;
      Max_Heartbeat_Retries        : aliased Long;
      Heartbeats_Per_Max_Samples   : aliased Long;
      Min_Nack_Response_Delay      : aliased Duration_T;
      Max_Nack_Response_Delay      : aliased Duration_T;
      Nack_Suppression_Duration    : aliased Duration_T;
      Max_Bytes_Per_Nack_Response  : aliased Long;
   end record;
   pragma Convention (C, RtpsReliableWriterProtocol_T);

   RTPS_RELIABLE_WRITER_PROTOCOL_DEFAULT :
   constant RtpsReliableWriterProtocol_T :=
                                             (Low_Watermark                 => 0,
                                              High_Watermark                => 1,
                                              Heartbeat_Period              => (3, 0),
                                              Fast_Heartbeat_Period         => (3, 0),
                                              Late_Joiner_Heartbeat_Period  => (3, 0),
                                              Max_Heartbeat_Retries         => 10,
                                              Heartbeats_Per_Max_Samples    => 8,
                                              Min_Nack_Response_Delay       => (0, 0),
                                              Max_Nack_Response_Delay       => (0, 200_000_000),
                                              Nack_Suppression_Duration     => (0, 0),
                                              Max_Bytes_Per_Nack_Response   => 131072);

   RTPS_RELIABLE_WRITER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT  :
   constant RtpsReliableWriterProtocol_T :=
                                                               (Low_Watermark                 => 0,
                                                                High_Watermark                => 1,
                                                                Heartbeat_Period              => (3, 0),
                                                                Fast_Heartbeat_Period         => (3, 0),
                                                                Late_Joiner_Heartbeat_Period  => (3, 0),
                                                                Max_Heartbeat_Retries         => 10,
                                                                Heartbeats_Per_Max_Samples    => 8,
                                                                Min_Nack_Response_Delay       => (0, 0),
                                                                Max_Nack_Response_Delay       => (0, 0),
                                                                Nack_Suppression_Duration     => (0, 0),
                                                                Max_Bytes_Per_Nack_Response   => 131072);

   type UserObjectSettings_T is record
      Size      : aliased Long;
      Alignment : aliased Long;
   end record;
   pragma Convention (C, UserObjectSettings_T);

   type TransportUnicastSettings_T is record
      Transports   : aliased String_Seq.Sequence;
      Receive_Port : aliased Long;
   end record;
   pragma Convention (C, TransportUnicastSettings_T);

   type TransportUnicastSettings_T_Access is access all TransportUnicastSettings_T;
   type TransportUnicastSettings_T_Array is array
     (Natural range <>) of aliased TransportUnicastSettings_T;
   procedure Initialize (Self  : in out TransportUnicastSettings_T);
   procedure Finalize (Self  : in out TransportUnicastSettings_T);
   procedure Copy (Dst : in out TransportUnicastSettings_T;
                   Src : in TransportUnicastSettings_T);
   package TransportUnicastSettings_Seq is new DDS_Support.Sequences_Generic
     (TransportUnicastSettings_T,
      TransportUnicastSettings_T_Access,
      DDS.Natural,
      1,
      TransportUnicastSettings_T_Array);

   type TransportMulticastSettings_T is record
      Transports      : String_Seq.Sequence;
      Receive_Address : DDS.String;
      Receive_Port    : Long;
   end record;
   pragma Convention (C, TransportMulticastSettings_T);

   type TransportMulticastSettings_T_Access is access all TransportMulticastSettings_T;
   type TransportMulticastSettings_T_Array is array
     (Natural range <>) of aliased TransportMulticastSettings_T;
   procedure Initialize (Self  : in out TransportMulticastSettings_T);
   procedure Finalize (Self  : in out TransportMulticastSettings_T);
   procedure Copy (Dst : in out TransportMulticastSettings_T;
                   Src : in TransportMulticastSettings_T);
   package TransportMulticastSettings_Seq is new DDS_Support.Sequences_Generic
     (TransportMulticastSettings_T,
      TransportMulticastSettings_T_Access,
      DDS.Natural,
      1,
      TransportMulticastSettings_T_Array);

   TRANSPORTSELECTION_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TRANSPORTSELECTION_QOS_POLICY_NAME,
                     "DDS_TRANSPORTSELECTION_QOS_POLICY_NAME");

   type TransportSelectionQosPolicy is record
      Enabled_Transports : aliased String_Seq.Sequence;
   end record;
   pragma Convention (C, TransportSelectionQosPolicy);

   TRANSPORT_SELECTION_QOS_POLICY_DEFAULT : constant TransportSelectionQosPolicy :=
                                              (Enabled_Transports => String_Seq.DEFAULT_SEQUENCE);

   TRANSPORTUNICAST_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TRANSPORTUNICAST_QOS_POLICY_NAME,
                     "DDS_TRANSPORTUNICAST_QOS_POLICY_NAME");

   type TransportUnicastQosPolicy is record
      Value :  TransportUnicastSettings_Seq.Sequence;
   end record;
   pragma Convention (C, TransportUnicastQosPolicy);

   TRANSPORT_UNICAST_QOS_POLICY_DEFAULT : constant TransportUnicastQosPolicy :=
                                            (Value => TransportUnicastSettings_Seq.DEFAULT_SEQUENCE);

   TRANSPORTMULTICAST_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TRANSPORTMULTICAST_QOS_POLICY_NAME,
                     "DDS_TRANSPORTMULTICAST_QOS_POLICY_NAME");

   type TransportMulticastQosPolicy is record
      Value : TransportMulticastSettings_Seq.Sequence;
   end record;
   pragma Convention (C, TransportMulticastQosPolicy);

   TRANSPORT_MULTICAST_QOS_POLICY_DEFAULT : constant TransportMulticastQosPolicy :=
                                              (Value => TransportMulticastSettings_Seq.DEFAULT_SEQUENCE);

   DISCOVERY_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DISCOVERY_QOS_POLICY_NAME, "DDS_DISCOVERY_QOS_POLICY_NAME");

   type DiscoveryQosPolicy is record
      Enabled_Transports             : aliased String_Seq.Sequence;
      Initial_Peers                  : aliased String_Seq.Sequence;
      Multicast_Receive_Addresses    : aliased String_Seq.Sequence;
      Metatraffic_Transport_Priority : aliased Long;
      Accept_Unknown_Peers           : aliased DDS.Boolean;
      Spare1                         : aliased DDS.Boolean;
      Spare2                         : aliased DDS.Boolean;
      Spare3                         : aliased DDS.Boolean;
   end record;
   pragma Convention (C, DiscoveryQosPolicy);

   DISCOVERY_QOS_POLICY_DEFAULT : constant  DiscoveryQosPolicy :=
                                    (Enabled_Transports             => String_Seq.DEFAULT_SEQUENCE,
                                     Initial_Peers                  => String_Seq.DEFAULT_SEQUENCE,
                                     Multicast_Receive_Addresses    => String_Seq.DEFAULT_SEQUENCE,
                                     Metatraffic_Transport_Priority => 0,
                                     Accept_Unknown_Peers           => TRUE,
                                     Spare1                         => False,
                                     Spare2                         => False,
                                     Spare3                         => False);

   type Discovery_ParticipantInformation is record
      Participant_Discovery_Id         : aliased Long;
      Participant_Discovery_Version    : aliased Long;
      Participant_Discovery_Vendor_Id  : aliased Long;
      Participant_Discovery_Parameters : aliased Octet_Seq.Sequence;
   end record;
   pragma Convention (C, Discovery_ParticipantInformation);

   type Discovery_ParticipantInformation_Access is access all Discovery_ParticipantInformation;
   type Discovery_ParticipantInformation_Array is array
     (Natural range <>) of aliased Discovery_ParticipantInformation;
   procedure Initialize (Self  : in out Discovery_ParticipantInformation);
   procedure Finalize (Self  : in out Discovery_ParticipantInformation);
   procedure Copy (Dst : in out Discovery_ParticipantInformation;
                   Src : in Discovery_ParticipantInformation);
   package Discovery_ParticipantInformationSeq is new DDS_Support.Sequences_Generic
     (Discovery_ParticipantInformation,
      Discovery_ParticipantInformation_Access,
      DDS.Natural,
      1,
      Discovery_ParticipantInformation_Array);

   type Discovery_EndpointInformation is record
      Endpoint_Discovery_Id         : aliased Long;
      Endpoint_Discovery_Version    : aliased Long;
      Endpoint_Discovery_Vendor_Id  : aliased Long;
      Endpoint_Discovery_Parameters : aliased Octet_Seq.Sequence;
   end record;
   pragma Convention (C, Discovery_EndpointInformation);

   type Discovery_EndpointInformation_Access is access all Discovery_EndpointInformation;
   type Discovery_EndpointInformation_Array is array
     (Natural range <>) of aliased Discovery_EndpointInformation;
   procedure Initialize (Self  : in out Discovery_EndpointInformation);
   procedure Finalize (Self  : in out Discovery_EndpointInformation);
   procedure Copy (Dst : in out Discovery_EndpointInformation;
                   Src : in Discovery_EndpointInformation);
   package Discovery_EndpointInformationSeq is new DDS_Support.Sequences_Generic
     (Discovery_EndpointInformation,
      Discovery_EndpointInformation_Access,
      DDS.Natural,
      1,
      Discovery_EndpointInformation_Array);

   TRANSPORTBUILTIN_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TRANSPORTBUILTIN_QOS_POLICY_NAME,
                     "DDS_TRANSPORTBUILTIN_QOS_POLICY_NAME");

   type TransportBuiltinKind is new Unsigned_Long;
   TRANSPORTBUILTIN_UDPv4 : constant TransportBuiltinKind := 2 ** 0;
   TRANSPORTBUILTIN_SHMEM : constant TransportBuiltinKind := 2 ** 1;
   TRANSPORTBUILTIN_INTRA : constant TransportBuiltinKind := 2 ** 2;
   TRANSPORTBUILTIN_UDPv6 : constant TransportBuiltinKind := 2 ** 3;

   TRANSPORTBUILTIN_INTRA_ALIAS : constant DDS.String;
   pragma Interface (C, TRANSPORTBUILTIN_INTRA_ALIAS, "DDS_TRANSPORTBUILTIN_INTRA_ALIAS");
   TRANSPORTBUILTIN_SHMEM_ALIAS : constant DDS.String;
   pragma Interface (C, TRANSPORTBUILTIN_SHMEM_ALIAS, "DDS_TRANSPORTBUILTIN_SHMEM_ALIAS");
   TRANSPORTBUILTIN_UDPv4_ALIAS : constant DDS.String;
   pragma Interface (C, TRANSPORTBUILTIN_UDPv4_ALIAS, "DDS_TRANSPORTBUILTIN_UDPv4_ALIAS");
   TRANSPORTBUILTIN_UDPv6_ALIAS : constant DDS.String;
   pragma Interface (C, TRANSPORTBUILTIN_UDPv6_ALIAS, "DDS_TRANSPORTBUILTIN_UDPv6_ALIAS");

   subtype TransportBuiltinKindMask is TransportBuiltinKind;

   TRANSPORTBUILTIN_MASK_NONE : constant TransportBuiltinKindMask := 0;
   TRANSPORTBUILTIN_MASK_DEFAULT : constant TransportBuiltinKindMask :=
                                     (TRANSPORTBUILTIN_UDPv4 or TRANSPORTBUILTIN_SHMEM);
   TRANSPORTBUILTIN_MASK_ALL  : constant TransportBuiltinKindMask := 16#FFFF_FFFF#;

   type TransportBuiltinQosPolicy is record
      Mask : TransportBuiltinKindMask;
   end record;
   pragma Convention (C, TransportBuiltinQosPolicy);

   TRANSPORT_BUILTIN_QOS_POLICY_DEFAULT : constant TransportBuiltinQosPolicy :=
                                            (Mask => TRANSPORTBUILTIN_MASK_DEFAULT);

   type RtpsWellKnownPorts_T is record
      Port_Base                     : aliased Long;
      Domain_Id_Gain                : aliased Long;
      Participant_Id_Gain           : aliased Long;
      Builtin_Multicast_Port_Offset : aliased Long;
      Builtin_Unicast_Port_Offset   : aliased Long;
      User_Multicast_Port_Offset    : aliased Long;
      User_Unicast_Port_Offset      : aliased Long;
   end record;
   pragma Convention (C, RtpsWellKnownPorts_T);

   RTI_BACKWARDS_COMPATIBLE_RTPS_WELL_KNOWN_PORTS : constant RtpsWellKnownPorts_T;
   pragma Interface (C, RTI_BACKWARDS_COMPATIBLE_RTPS_WELL_KNOWN_PORTS,
                     "DDS_RTI_BACKWARDS_COMPATIBLE_RTPS_WELL_KNOWN_PORTS");
   INTEROPERABLE_RTPS_WELL_KNOWN_PORTS : constant RtpsWellKnownPorts_T;
   pragma Interface (C, INTEROPERABLE_RTPS_WELL_KNOWN_PORTS,
                     "DDS_INTEROPERABLE_RTPS_WELL_KNOWN_PORTS");

   RTPS_WELL_KNOWN_PORTS_DEFAULT : constant RtpsWellKnownPorts_T :=
                                     (Port_Base                     => 7400,
                                      Domain_Id_Gain                => 10,
                                      Participant_Id_Gain           => 1000,
                                      Builtin_Multicast_Port_Offset => 2,
                                      Builtin_Unicast_Port_Offset   => 0,
                                      User_Multicast_Port_Offset    => 1,
                                      User_Unicast_Port_Offset      => 3);

   WIREPROTOCOL_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, WIREPROTOCOL_QOS_POLICY_NAME, "DDS_WIREPROTOCOL_QOS_POLICY_NAME");

   type WireProtocolQosPolicy is record
      Participant_Id        : aliased Long;
      Rtps_Host_Id          : aliased Unsigned_Long;
      Rtps_App_Id           : aliased Unsigned_Long;
      Rtps_Instance_Id      : aliased Unsigned_Long;
      Rtps_Well_Known_Ports : aliased RtpsWellKnownPorts_T;
   end record;
   pragma Convention (C, WireProtocolQosPolicy);

   RTPS_AUTO_ID : constant Unsigned_Long := 0;
   WIRE_PROTOCOL_QOS_POLICY_DEFAULT : constant WireProtocolQosPolicy :=
                                        (Participant_Id        => -1, -- Automatic
                                         Rtps_Host_Id          => RTPS_AUTO_ID,
                                         Rtps_App_Id           => RTPS_AUTO_ID,
                                         Rtps_Instance_Id      => RTPS_AUTO_ID,
                                         Rtps_Well_Known_Ports => RTPS_WELL_KNOWN_PORTS_DEFAULT);


   LOCATOR_ADDRESS_LENGTH_MAX  : constant Unsigned_Long := 16;
   type Locator_Address_Array_T is array (0 .. LOCATOR_ADDRESS_LENGTH_MAX - 1) of Octet;

   type Locator_T is record
      Kind    : aliased Long;
      Port    : aliased Unsigned_Long;
      Address : aliased Locator_Address_Array_T;
   end record;
   pragma Convention (C,  Locator_T);

   type Locator_T_Access is access all Locator_T;
   type Locator_T_Array is array (Natural range <>) of aliased Locator_T;
   procedure Initialize (Self  : in out Locator_T);
   procedure Finalize (Self  : in out Locator_T);
   procedure Copy (Dst : in out Locator_T; Src : in Locator_T);
   package LocatorSeq is new DDS_Support.Sequences_Generic
     (Locator_T,
      Locator_T_Access,
      DDS.Natural,
      1,
      Locator_T_Array);

   LOCATOR_INVALID : constant Locator_T;
   pragma Interface (C, LOCATOR_INVALID, "DDS_LOCATOR_INVALID");
   LOCATOR_KIND_INVALID : constant Long;
   pragma Interface (C, LOCATOR_KIND_INVALID, "DDS_LOCATOR_KIND_INVALID");
   LOCATOR_PORT_INVALID : constant Unsigned_Long;
   pragma Interface (C, LOCATOR_PORT_INVALID, "DDS_LOCATOR_PORT_INVALID");
   LOCATOR_ADDRESS_INVALID : constant Locator_Address_Array_T;
   pragma Interface (C, LOCATOR_ADDRESS_INVALID, "DDS_LOCATOR_ADDRESS_INVALID");
   LOCATOR_KIND_UDPv4 : constant Long;
   pragma Interface (C, LOCATOR_KIND_UDPv4, "DDS_LOCATOR_KIND_UDPv4");
   LOCATOR_KIND_UDPv6 : constant Long;
   pragma Interface (C, LOCATOR_KIND_UDPv6, "DDS_LOCATOR_KIND_UDPv6");
   LOCATOR_KIND_RESERVED : constant Long;
   pragma Interface (C, LOCATOR_KIND_RESERVED, "DDS_LOCATOR_KIND_RESERVED");
   LOCATOR_KIND_SHMEM  : constant Long;
   pragma Interface (C, LOCATOR_KIND_SHMEM, "DDS_LOCATOR_KIND_SHMEM");

   type ProtocolVersion_T  is record
      Major : aliased Octet;
      Minor : aliased Octet;
   end record;
   pragma Convention (C, ProtocolVersion_T);

   PROTOCOL_VERSION_DEFAULT : constant ProtocolVersion_T :=
                                (Major => 0,
                                 Minor => 0);

   PROTOCOLVERSION_1_0 : constant ProtocolVersion_T :=
                           (Major => 1,
                            Minor => 0);

   PROTOCOLVERSION_1_1 : constant ProtocolVersion_T :=
                           (Major => 1,
                            Minor => 1);

   PROTOCOLVERSION_1_2 : constant ProtocolVersion_T :=
                           (Major => 1,
                            Minor => 2);

   PROTOCOLVERSION_2_0 : constant ProtocolVersion_T :=
                           (Major => 2,
                            Minor => 0);

   PROTOCOLVERSION_2_1 : constant ProtocolVersion_T :=
                           (Major => 2,
                            Minor => 1);

   PROTOCOLVERSION : constant ProtocolVersion_T :=
                       (Major => 2,
                        Minor => 1);

   VENDOR_ID_LENGTH_MAX : constant := 2;
   type VendorId_Array_T is array (0 .. VENDOR_ID_LENGTH_MAX - 1) of Octet;

   type VendorId_T is record
      VendorId : aliased VendorId_Array_T;
   end record;
   pragma Convention (C, VendorId_T);

   VENDOR_ID_DEFAULT : constant  VendorId_T := (VendorId => (0, 0));
   VENDORID_UNKNOWN : constant  VendorId_T := (VendorId => (0, 0));

   type ProductVersion_T is record
      Major    : aliased char;
      Minor    : aliased char;
      Release  : aliased char;
      Revision : aliased char;
   end record;
   pragma Convention (C, ProductVersion_T);

   PRODUCTVERSION_UNKNOWN : constant ProductVersion_T :=
                              (Major    => Char (ASCII.nul),
                               Minor    => Char (ASCII.nul),
                               Release  => '0',
                               Revision => Char (ASCII.nul));

   DATAREADERRESOURCELIMITS_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DATAREADERRESOURCELIMITS_QOS_POLICY_NAME,
                     "DDS_DATAREADERRESOURCELIMITS_QOS_POLICY_NAME");

   type DataReaderResourceLimitsQosPolicy is record
      Max_Remote_Writers                       : aliased Long;
      Max_Remote_Writers_Per_Instance          : aliased Long;
      Max_Samples_Per_Remote_Writer            : aliased Long;
      Max_Infos                                : aliased Long;
      Initial_Remote_Writers                   : aliased Long;
      Initial_Remote_Writers_Per_Instance      : aliased Long;
      Initial_Infos                            : aliased Long;
      Initial_Outstanding_Reads                : aliased Long;
      Max_Outstanding_Reads                    : aliased Long;
      Max_Samples_Per_Read                     : aliased Long;
      Disable_Fragmentation_Support            : aliased DDS.Boolean;
      Spare1                                   : aliased DDS.Boolean;
      Spare2                                   : aliased DDS.Boolean;
      Spare3                                   : aliased DDS.Boolean;
      Max_Fragmented_Samples                   : aliased Long;
      Initial_Fragmented_Samples               : aliased Long;
      Max_Fragmented_Samples_Per_Remote_Writer : aliased Long;
      Max_Fragments_Per_Sample                 : aliased Long;
      Dynamically_Allocate_Fragmented_Samples  : aliased DDS.Boolean;
      Spare4                                   : aliased DDS.Boolean;
      Spare5                                   : aliased DDS.Boolean;
      Spare6                                   : aliased DDS.Boolean;
   end record;
   pragma Convention (C, DataReaderResourceLimitsQosPolicy);

   DATA_READER_RESOURCE_LIMITS_QOS_POLICY_DEFAULT :
   constant DataReaderResourceLimitsQosPolicy :=
                                                      (Max_Remote_Writers                       => -1,
                                                       Max_Remote_Writers_Per_Instance          => -1,
                                                       Max_Samples_Per_Remote_Writer            => -1,
                                                       Max_Infos                                => -1,
                                                       Initial_Remote_Writers                   =>  2,
                                                       Initial_Remote_Writers_Per_Instance      => 2,
                                                       Initial_Infos                            => 32,
                                                       Initial_Outstanding_Reads                => 2,
                                                       Max_Outstanding_Reads                    =>  -1,
                                                       Max_Samples_Per_Read                     =>  1024,
                                                       Disable_Fragmentation_Support            =>  FALSE,
                                                       Spare1                                   =>  FALSE,
                                                       Spare2                                   =>  FALSE,
                                                       Spare3                                   =>  FALSE,
                                                       Max_Fragmented_Samples                   =>  1024,
                                                       Initial_Fragmented_Samples               =>  4,
                                                       Max_Fragmented_Samples_Per_Remote_Writer =>  256,
                                                       Max_Fragments_Per_Sample                 =>  512,
                                                       Dynamically_Allocate_Fragmented_Samples  => FALSE,
                                                       Spare4                                   => FALSE,
                                                       Spare5                                   => FALSE,
                                                       Spare6                                   => FALSE);

   DATAWRITERRESOURCELIMITS_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DATAWRITERRESOURCELIMITS_QOS_POLICY_NAME,
                     "DDS_DATAWRITERRESOURCELIMITS_QOS_POLICY_NAME");

   type DataWriterResourceLimitsQosPolicy is record
      Initial_Concurrent_Blocking_Threads : aliased Long;
      Max_Concurrent_Blocking_Threads     : aliased Long;
      Max_Remote_Reader_Filters           : aliased Long;
   end record;
   pragma Convention (C, DataWriterResourceLimitsQosPolicy);

   DATA_WRITER_RESOURCE_LIMITS_QOS_POLICY_DEFAULT :
   constant DataWriterResourceLimitsQosPolicy :=
                                                      (Initial_Concurrent_Blocking_Threads => 1,
                                                       Max_Concurrent_Blocking_Threads     => -1,
                                                       Max_Remote_Reader_Filters           => 32);


   STATISTICS_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, STATISTICS_QOS_POLICY_NAME,
                     "DDS_STATISTICS_QOS_POLICY_NAME");

   type StatisticsQosPolicy is record
      Gather_Statistics : DDS.Boolean;
   end record;
   pragma Convention (C, StatisticsQosPolicy);

   STATISTICS_QOS_POLICY_DEFAULT : constant StatisticsQosPolicy :=
                                     (Gather_Statistics => False);


   SERVICE_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, SERVICE_QOS_POLICY_NAME,
                     "DDS_SERVICE_QOS_POLICY_NAME");

   type ServiceQosPolicyKind is
     (
      NO_SERVICE_QOS,
      PERSISTENCE_SERVICE_QOS
     );
   pragma Convention (C, ServiceQosPolicyKind);

   type ServiceQosPolicy is record
      Kind : ServiceQosPolicyKind;
   end record;
   pragma Convention (C, ServiceQosPolicy);

   SERVICE_QOS_POLICY_DEFAULT : constant ServiceQosPolicy :=
                                  (Kind => NO_SERVICE_QOS);

   DATAREADERPROTOCOL_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DATAREADERPROTOCOL_QOS_POLICY_NAME,
                     "DDS_DATAREADERPROTOCOL_QOS_POLICY_NAME");

   type DataReaderProtocolQosPolicy is record
      Virtual_Guid         : aliased Guid_T;
      Rtps_Object_Id       : aliased Unsigned_Long;
      Rtps_Reliable_Reader : aliased RtpsReliableReaderProtocol_T;
      Expects_Inline_Qos   : aliased DDS.Boolean;
      Spare1               : aliased DDS.Boolean;
      Spare2               : aliased DDS.Boolean;
      Spare3               : aliased DDS.Boolean;
   end record;
   pragma Convention (C, DataReaderProtocolQosPolicy);

   DATA_READER_PROTOCOL_QOS_POLICY_DEFAULT : constant DataReaderProtocolQosPolicy :=
                                               (Virtual_Guid         => (Value => (others => 0)),
                                                Rtps_Object_Id       => RTPS_AUTO_ID,
                                                Rtps_Reliable_Reader => RTPS_RELIABLE_READER_PROTOCOL_DEFAULT,
                                                Expects_Inline_Qos   => FALSE,
                                                Spare1               => FALSE,
                                                Spare2               => FALSE,
                                                Spare3               => FALSE);

   DATAWRITERPROTOCOL_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DATAWRITERPROTOCOL_QOS_POLICY_NAME,
                     "DDS_DATAWRITERPROTOCOL_QOS_POLICY_NAME");

   type DataWriterProtocolQosPolicy is record
      Virtual_Guid         : aliased Guid_T;
      Rtps_Object_Id       : aliased Unsigned_Long;
      Push_On_Write        : aliased DDS.Boolean;
      Spare1               : aliased DDS.Boolean;
      Spare2               : aliased DDS.Boolean;
      Spare3               : aliased DDS.Boolean;
      Rtps_Reliable_Writer : aliased RtpsReliableWriterProtocol_T;
   end record;
   pragma Convention (C, DataWriterProtocolQosPolicy);

   DATA_WRITER_PROTOCOL_QOS_POLICY_DEFAULT : constant DataWriterProtocolQosPolicy :=
                                               (Virtual_Guid         => (Value => (others => 0)),
                                                Rtps_Object_Id       => RTPS_AUTO_ID,
                                                Push_On_Write        => TRUE,
                                                Spare1               => FALSE,
                                                Spare2               => FALSE,
                                                Spare3               => FALSE,
                                                Rtps_Reliable_Writer => RTPS_RELIABLE_WRITER_PROTOCOL_DEFAULT);

   SYSTEMRESOURCELIMITS_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, SYSTEMRESOURCELIMITS_QOS_POLICY_NAME,
                     "DDS_SYSTEMRESOURCELIMITS_QOS_POLICY_NAME");

   type SystemResourceLimitsQosPolicy is record
      Max_Objects_Per_Thread : Long;
   end record;
   pragma Convention (C, SystemResourceLimitsQosPolicy);

   SYSTEM_RESOURCE_LIMITS_QOS_POLICY_DEFAULT : constant SystemResourceLimitsQosPolicy :=
                                                 (Max_Objects_Per_Thread => 512);

   DOMAINPARTICIPANTRESOURCELIMITS_QOS_POLICY_NAME :  constant DDS.String;
   pragma Interface (C, DOMAINPARTICIPANTRESOURCELIMITS_QOS_POLICY_NAME,
                     "DDS_DOMAINPARTICIPANTRESOURCELIMITS_QOS_POLICY_NAME");

   type DomainParticipantResourceLimitsQosPolicy is record
      Local_Writer_Allocation                    : aliased AllocationSettings_T;
      Local_Reader_Allocation                    : aliased AllocationSettings_T;
      Local_Publisher_Allocation                 : aliased AllocationSettings_T;
      Local_Subscriber_Allocation                : aliased AllocationSettings_T;
      Local_Topic_Allocation                     : aliased AllocationSettings_T;
      Remote_Writer_Allocation                   : aliased AllocationSettings_T;
      Remote_Reader_Allocation                   : aliased AllocationSettings_T;
      Remote_Participant_Allocation              : aliased AllocationSettings_T;
      Matching_Writer_Reader_Pair_Allocation     : aliased AllocationSettings_T;
      Matching_Reader_Writer_Pair_Allocation     : aliased AllocationSettings_T;
      Ignored_Entity_Allocation                  : aliased AllocationSettings_T;
      Content_Filtered_Topic_Allocation          : aliased AllocationSettings_T;
      Content_Filter_Allocation                  : aliased AllocationSettings_T;
      Read_Condition_Allocation                  : aliased AllocationSettings_T;
      Outstanding_Asynchronous_Sample_Allocation : aliased AllocationSettings_T;
      Flow_Controller_Allocation                 : aliased AllocationSettings_T;
      Local_Writer_Hash_Buckets                  : aliased Long;
      Local_Reader_Hash_Buckets                  : aliased Long;
      Local_Publisher_Hash_Buckets               : aliased Long;
      Local_Subscriber_Hash_Buckets              : aliased Long;
      Local_Topic_Hash_Buckets                   : aliased Long;
      Remote_Writer_Hash_Buckets                 : aliased Long;
      Remote_Reader_Hash_Buckets                 : aliased Long;
      Remote_Participant_Hash_Buckets            : aliased Long;
      Matching_Writer_Reader_Pair_Hash_Buckets   : aliased Long;
      Matching_Reader_Writer_Pair_Hash_Buckets   : aliased Long;
      Ignored_Entity_Hash_Buckets                : aliased Long;
      Content_Filtered_Topic_Hash_Buckets        : aliased Long;
      Content_Filter_Hash_Buckets                : aliased Long;
      Flow_Controller_Hash_Buckets               : aliased Long;
      Max_Gather_Destinations                    : aliased Long;
      Participant_User_Data_Max_Length           : aliased Long;
      Inter_Participant_Data_Max_Length          : aliased Long;
      Topic_Data_Max_Length                      : aliased Long;
      Publisher_Group_Data_Max_Length            : aliased Long;
      Subscriber_Group_Data_Max_Length           : aliased Long;
      Writer_User_Data_Max_Length                : aliased Long;
      Reader_User_Data_Max_Length                : aliased Long;
      Max_Partitions                             : aliased Long;
      Max_Partition_Cumulative_Characters        : aliased Long;
      Default_Partition_Matches_All              : aliased DDS.Boolean;
      Allow_No_Partitions                        : aliased DDS.Boolean;
      Spare1                                     : aliased DDS.Boolean;
      Spare2                                     : aliased DDS.Boolean;
      Type_Code_Max_Serialized_Length            : aliased Long;
      Contentfilter_Property_Max_Length          : aliased Long;
      Participant_Property_List_Max_Length       : aliased Long;
      Participant_Property_String_Max_Length     : aliased Long;
      Writer_Property_List_Max_Length            : aliased Long;
      Writer_Property_String_Max_Length          : aliased Long;
      Reader_Property_List_Max_Length            : aliased Long;
      Reader_Property_String_Max_Length          : aliased Long;
      Plugin_Info_Parameter_Max_Length           : aliased Long;
   end record;
   pragma Convention (C, DomainParticipantResourceLimitsQosPolicy);

   DomainParticipantResourceLimitsQosPolicy_MATCH_INIT : constant DDS.Long := 32;

   DOMAIN_PARTICIPANT_RESOURCE_LIMITS_QOS_POLICY_DEFAULT :
   constant DomainParticipantResourceLimitsQosPolicy :=
                                                             (Local_Writer_Allocation                    => (16, -1, -1),
                                                              Local_Reader_Allocation                    => (16, -1, -1),
                                                              Local_Publisher_Allocation                 => (4, -1, -1),
                                                              Local_Subscriber_Allocation                => (4, -1, -1),
                                                              Local_Topic_Allocation                     => (16, -1, -1),
                                                              Remote_Writer_Allocation                   => (64, -1, -1),
                                                              Remote_Reader_Allocation                   => (64, -1, -1),
                                                              Remote_Participant_Allocation              => (16, -1, -1),
                                                              Matching_Writer_Reader_Pair_Allocation     =>
                                                                (DomainParticipantResourceLimitsQosPolicy_MATCH_INIT, -1, -1),
                                                              Matching_Reader_Writer_Pair_Allocation     =>
                                                                (DomainParticipantResourceLimitsQosPolicy_MATCH_INIT, -1, -1),
                                                              Ignored_Entity_Allocation                  => (8, -1, -1),
                                                              Content_Filtered_Topic_Allocation          => (4, -1, -1),
                                                              Content_Filter_Allocation                  => (4, -1, -1),
                                                              Read_Condition_Allocation                  => (4, -1, -1),
                                                              Outstanding_Asynchronous_Sample_Allocation => (64, -1, -1),
                                                              Flow_Controller_Allocation                 => (4, -1, -1),
                                                              Local_Writer_Hash_Buckets                  => 4,
                                                              Local_Reader_Hash_Buckets                  => 4,
                                                              Local_Publisher_Hash_Buckets               => 1,
                                                              Local_Subscriber_Hash_Buckets              => 1,
                                                              Local_Topic_Hash_Buckets                   => 4,
                                                              Remote_Writer_Hash_Buckets                 => 16,
                                                              Remote_Reader_Hash_Buckets                 => 16,
                                                              Remote_Participant_Hash_Buckets            => 4,
                                                              Matching_Writer_Reader_Pair_Hash_Buckets   => 32,
                                                              Matching_Reader_Writer_Pair_Hash_Buckets   => 32,
                                                              Ignored_Entity_Hash_Buckets                => 1,
                                                              Content_Filtered_Topic_Hash_Buckets        => 1,
                                                              Content_Filter_Hash_Buckets                => 1,
                                                              Flow_Controller_Hash_Buckets               => 1,
                                                              Max_Gather_Destinations                    => 8,
                                                              Participant_User_Data_Max_Length           => 256,
                                                              Inter_Participant_Data_Max_Length          => 256,
                                                              Topic_Data_Max_Length                      => 256,
                                                              Publisher_Group_Data_Max_Length            => 256,
                                                              Subscriber_Group_Data_Max_Length           => 256,
                                                              Writer_User_Data_Max_Length                => 256,
                                                              Reader_User_Data_Max_Length                => 256,
                                                              Max_Partitions                             => 64,
                                                              Max_Partition_Cumulative_Characters        => 256,
                                                              Default_Partition_Matches_All              => FALSE,
                                                              Allow_No_Partitions                        => FALSE,
                                                              Spare1                                     => FALSE,
                                                              Spare2                                     => FALSE,
                                                              Type_Code_Max_Serialized_Length            => 2048,
                                                              Contentfilter_Property_Max_Length          => 256,
                                                              Participant_Property_List_Max_Length       => 32,
                                                              Participant_Property_String_Max_Length     => 1024,
                                                              Writer_Property_List_Max_Length            => 32,
                                                              Writer_Property_String_Max_Length          => 1024,
                                                              Reader_Property_List_Max_Length            => 32,
                                                              Reader_Property_String_Max_Length          => 1024,
                                                              Plugin_Info_Parameter_Max_Length           => 256);
   EVENT_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, EVENT_QOS_POLICY_NAME, "DDS_EVENT_QOS_POLICY_NAME");

   type EventQosPolicy is record
      Thread        : aliased ThreadSettings_T;
      Initial_Count : aliased Long;
      Max_Count     : aliased Long;
   end record;
   pragma Convention (C, EventQosPolicy);

   EVENT_QOS_POLICY_DEFAULT : constant EventQosPolicy :=
                                (Thread        => THREAD_SETTINGS_DEFAULT,
                                 Initial_Count => 256,
                                 Max_Count     => -1);

   DATABASE_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DATABASE_QOS_POLICY_NAME, "DDS_DATABASE_QOS_POLICY_NAME");

   type DatabaseQosPolicy is record
      Thread                      : aliased ThreadSettings_T;
      Shutdown_Timeout            : aliased Duration_T;
      Cleanup_Period              : aliased Duration_T;
      Shutdown_Cleanup_Period     : aliased Duration_T;
      Initial_Records             : aliased Long;
      Max_Skiplist_Level          : aliased Long;
      Table_Allocation_Block_Size : aliased Long;
      Max_Weak_References         : aliased Long;
      Initial_Weak_References     : aliased Long;
   end record;
   pragma Convention (C, DatabaseQosPolicy);

   DATABASE_QOS_POLICY_DEFAULT :  constant DatabaseQosPolicy :=
                                   (Thread                      => THREAD_SETTINGS_DEFAULT,
                                    Shutdown_Timeout            => (15, 0),
                                    Cleanup_Period              => (61, 0),
                                    Shutdown_Cleanup_Period     => (1, 0),
                                    Initial_Records             => 1024,
                                    Max_Skiplist_Level          => 7,
                                    Table_Allocation_Block_Size => 48,
                                    Max_Weak_References         => -1,
                                    Initial_Weak_References     => 2049);

   RECEIVERPOOL_QOS_POLICY_NAME : DDS.String;
   pragma Interface (C, RECEIVERPOOL_QOS_POLICY_NAME, "DDS_RECEIVERPOOL_QOS_POLICY_NAME");

   type ReceiverPoolQosPolicy is record
      Thread                  : aliased ThreadSettings_T;
      Initial_Receive_Threads : aliased Long;
      Max_Receive_Threads     : aliased Long;
      Buffer_Size             : aliased Long;
      Buffer_Alignment        : aliased Long;
      Is_Timestamp_Enabled    : aliased DDS.Boolean;
      Spare1                  : aliased DDS.Boolean;
      Spare2                  : aliased DDS.Boolean;
      Spare3                  : aliased DDS.Boolean;
   end record;
   pragma Convention (C, ReceiverPoolQosPolicy);

   ReceiverPoolQosPolicy_MAX_RECEIVE_THREADS_DEFAULT : constant Long := -1;

   RECEIVER_POOL_QOS_POLICY_DEFAULT : constant ReceiverPoolQosPolicy :=
                                        (Thread                  => THREAD_SETTINGS_DEFAULT,
                                         Initial_Receive_Threads => 4,
                                         Max_Receive_Threads     => ReceiverPoolQosPolicy_MAX_RECEIVE_THREADS_DEFAULT,
                                         Buffer_Size             => 9216,
                                         Buffer_Alignment        => 16,
                                         Is_Timestamp_Enabled    => TRUE,
                                         Spare1                  => FALSE,
                                         Spare2                  => FALSE,
                                         Spare3                  => FALSE);

   type BuiltinTopicReaderResourceLimits_T is record
      Initial_Samples           : aliased Long;
      Max_Samples               : aliased Long;
      Initial_Infos             : aliased Long;
      Max_Infos                 : aliased Long;
      Initial_Outstanding_Reads : aliased Long;
      Max_Outstanding_Reads     : aliased Long;
      Max_Samples_Per_Read      : aliased Long;
   end record;
   pragma Convention (C, BuiltinTopicReaderResourceLimits_T);

   BUILTIN_TOPIC_READER_RESOURCE_LIMITS_DEFAULT : constant BuiltinTopicReaderResourceLimits_T :=
                                                    (Initial_Samples           => 64,
                                                     Max_Samples               => -1,
                                                     Initial_Infos             => 64,
                                                     Max_Infos                 => -1,
                                                     Initial_Outstanding_Reads => 2,
                                                     Max_Outstanding_Reads     => -1,
                                                     Max_Samples_Per_Read      => 1024);



   BUILTIN_TOPIC_KEY_TYPE_NATIVE_LENGTH : constant := 4;
   type  BuiltinTopicKey_Array_T is array
     (0 .. BUILTIN_TOPIC_KEY_TYPE_NATIVE_LENGTH - 1) of Builtin_Topic_Key_Type_Native;

   type BuiltinTopicKey_T is record
      Value : BuiltinTopicKey_Array_T;
   end record;
   pragma Convention (C, BuiltinTopicKey_T);
   type BuiltinTopicKey_T_Access is access all BuiltinTopicKey_T;

   BuiltinTopicKey_T_INITIALIZER : constant BuiltinTopicKey_T :=
                                     (Value => (0, 0, 0, 0));

   function BuiltinTopicKey_Equals (A : in BuiltinTopicKey_T_Access;
                                    B : in BuiltinTopicKey_T_Access)
                                    return DDS.Boolean;

   PUBLISHMODE_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, PUBLISHMODE_QOS_POLICY_NAME, "DDS_PUBLISHMODE_QOS_POLICY_NAME");

   type PublishModeQosPolicyKind is
     (SYNCHRONOUS_PUBLISH_MODE_QOS,
      ASYNCHRONOUS_PUBLISH_MODE_QOS);
   pragma Convention (C, PublishModeQosPolicyKind);

   type PublishModeQosPolicy is record
      Kind                 : aliased PublishModeQosPolicyKind;
      Flow_Controller_Name : aliased DDS.String;
   end record;
   pragma Convention (C, PublishModeQosPolicy);

   PUBLISH_MODE_QOS_POLICY_DEFAULT : constant PublishModeQosPolicy :=
                                       (Kind                 => SYNCHRONOUS_PUBLISH_MODE_QOS,
                                        Flow_Controller_Name => NULL_STRING);


   DISCOVERYCONFIG_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, DISCOVERYCONFIG_QOS_POLICY_NAME,
                     "DDS_DISCOVERYCONFIG_QOS_POLICY_NAME");

   type DiscoveryConfigBuiltinPluginKind is new DDS.Unsigned_Long;
   subtype DiscoveryConfigBuiltinPluginKindMask is  DiscoveryConfigBuiltinPluginKind;


   DISCOVERYCONFIG_BUILTIN_SPDP : constant  DiscoveryConfigBuiltinPluginKindMask := 2 ** 0;
   DISCOVERYCONFIG_BUILTIN_SEDP : constant  DiscoveryConfigBuiltinPluginKindMask := 2 ** 1;
   DISCOVERYCONFIG_BUILTIN_SDP  : constant  DiscoveryConfigBuiltinPluginKindMask :=
                                    (DISCOVERYCONFIG_BUILTIN_SPDP or DISCOVERYCONFIG_BUILTIN_SEDP);

   DISCOVERYCONFIG_BUILTIN_EDS  : constant  DiscoveryConfigBuiltinPluginKindMask := 2 ** 2;
   DISCOVERYCONFIG_BUILTIN_PLUGIN_MASK_ALL     :
   constant  DiscoveryConfigBuiltinPluginKindMask := 16#EFFF#;
   DISCOVERYCONFIG_BUILTIN_PLUGIN_MASK_NONE    :
   constant  DiscoveryConfigBuiltinPluginKindMask := 0;
   DISCOVERYCONFIG_BUILTIN_PLUGIN_MASK_DEFAULT :
   constant  DiscoveryConfigBuiltinPluginKindMask := DISCOVERYCONFIG_BUILTIN_SDP;

   type DiscoveryPluginPromiscuityKind is
     (DISCOVERYPLUGIN_DISCOVER_MATCHING_REMOTE_ENTITIES_PROMISCUITY,
      DISCOVERYPLUGIN_DISCOVER_ALL_REMOTE_ENTITIES_PROMISCUITY);
   pragma Convention (C, DiscoveryPluginPromiscuityKind);

   for DiscoveryPluginPromiscuityKind use
     (DISCOVERYPLUGIN_DISCOVER_MATCHING_REMOTE_ENTITIES_PROMISCUITY => 1,
      DISCOVERYPLUGIN_DISCOVER_ALL_REMOTE_ENTITIES_PROMISCUITY      => 16#FFFF#);

   type RemoteParticipantPurgeKind is
     (LIVELINESS_BASED_REMOTE_PARTICIPANT_PURGE,
      NO_REMOTE_PARTICIPANT_PURGE);
   pragma Convention (C, RemoteParticipantPurgeKind);

   for RemoteParticipantPurgeKind use
     (LIVELINESS_BASED_REMOTE_PARTICIPANT_PURGE => 0,
      NO_REMOTE_PARTICIPANT_PURGE               => 1);

   type DiscoveryBuiltinReaderFragmentationResourceLimits_T is record
      Disable_Fragmentation_Support            : aliased DDS.Boolean;
      Spare1                                   : aliased DDS.Boolean;
      Spare2                                   : aliased DDS.Boolean;
      Spare3                                   : aliased DDS.Boolean;
      Max_Fragmented_Samples                   : aliased Long;
      Initial_Fragmented_Samples               : aliased Long;
      Max_Fragmented_Samples_Per_Remote_Writer : aliased Long;
      Max_Fragments_Per_Sample                 : aliased Long;
      Dynamically_Allocate_Fragmented_Samples  : aliased DDS.Boolean;
      Spare4                                   : aliased DDS.Boolean;
      Spare5                                   : aliased DDS.Boolean;
      Spare6                                   : aliased DDS.Boolean;
   end record;
   pragma Convention (C, DiscoveryBuiltinReaderFragmentationResourceLimits_T);

   DISCOVERY_BUILTIN_READER_FRAGMENTATION_RESOURCE_LIMITS_DEFAULT :
   constant DiscoveryBuiltinReaderFragmentationResourceLimits_T :=
                                                                      (Disable_Fragmentation_Support            => FALSE,
                                                                       Spare1                                   => FALSE,
                                                                       Spare2                                   => FALSE,
                                                                       Spare3                                   => FALSE,
                                                                       Max_Fragmented_Samples                   => 1024,
                                                                       Initial_Fragmented_Samples               => 4,
                                                                       Max_Fragmented_Samples_Per_Remote_Writer => 256,
                                                                       Max_Fragments_Per_Sample                 => 512,
                                                                       Dynamically_Allocate_Fragmented_Samples  => FALSE,
                                                                       Spare4                                   => FALSE,
                                                                       Spare5                                   => FALSE,
                                                                       Spare6                                   => FALSE);

   type DiscoveryConfigQosPolicy is record

      Participant_Liveliness_Lease_Duration                  : aliased Duration_T;
      Participant_Liveliness_Assert_Period                   : aliased Duration_T;
      Remote_Participant_Purge_Kind                          : aliased RemoteParticipantPurgeKind;
      Max_Liveliness_Loss_Detection_Period                   : aliased Duration_T;
      Initial_Participant_Announcements                      : aliased Long;
      Min_Initial_Participant_Announcement_Period            : aliased Duration_T;
      Max_Initial_Participant_Announcement_Period            : aliased Duration_T;
      Participant_Reader_Resource_Limits                     : aliased BuiltinTopicReaderResourceLimits_T;
      Publication_Reader                                     : aliased RtpsReliableReaderProtocol_T;
      Publication_Reader_Resource_Limits                     : aliased BuiltinTopicReaderResourceLimits_T;
      Subscription_Reader                                    : aliased RtpsReliableReaderProtocol_T;
      Subscription_Reader_Resource_Limits                    : aliased BuiltinTopicReaderResourceLimits_T;
      Publication_Writer                                     : aliased RtpsReliableWriterProtocol_T;
      Subscription_Writer                                    : aliased RtpsReliableWriterProtocol_T;
      Endpoint_Plugin_Redundancy_Level                       : aliased Long;
      Builtin_Discovery_Plugins                              : aliased DiscoveryConfigBuiltinPluginKindMask;

      --   /***************** HIDDEN FROM USER ********************************/

      Publication_Reader_Fragmentation_Resource_Limits       :
      aliased DiscoveryBuiltinReaderFragmentationResourceLimits_T;
      Subscription_Reader_Fragmentation_Resource_Limits      :
      aliased DiscoveryBuiltinReaderFragmentationResourceLimits_T;
      Sedp_Rely_On_Spdp_Only                                 : aliased DDS.Boolean;
      Spare1                                                 : aliased DDS.Boolean;
      Spare2                                                 : aliased DDS.Boolean;
      Spare3                                                 : aliased DDS.Boolean;
      Inter_Participant_Reader                               : aliased RtpsReliableReaderProtocol_T;
      Inter_Participant_Writer                               : aliased RtpsReliableWriterProtocol_T;
      Publication_Writer_Latency_Budget                      : aliased LatencyBudgetQosPolicy;
      Publication_Writer_Push_On_Write                       : aliased DDS.Boolean;
      Spare4                                                 : aliased DDS.Boolean;
      Spare5                                                 : aliased DDS.Boolean;
      Spare6                                                 : aliased DDS.Boolean;
      Publication_Writer_Publish_Mode                        : aliased PublishModeQosPolicy;
      Subscription_Writer_Latency_Budget                     : aliased LatencyBudgetQosPolicy;
      Subscription_Writer_Push_On_Write                      : aliased DDS.Boolean;
      Spare7                                                 : aliased DDS.Boolean;
      Spare8                                                 : aliased DDS.Boolean;
      Spare9                                                 : aliased DDS.Boolean;
      Subscription_Writer_Publish_Mode                       : aliased PublishModeQosPolicy;
      Participant_State_Writer                               : aliased RtpsReliableWriterProtocol_T;
      Participant_State_Writer_Latency_Budget                : aliased LatencyBudgetQosPolicy;
      Participant_State_Writer_Push_On_Write                 : aliased DDS.Boolean;
      Spare10                                                : aliased DDS.Boolean;
      Spare11                                                : aliased DDS.Boolean;
      Spare12                                                : aliased DDS.Boolean;
      Participant_State_Writer_Publish_Mode                  : aliased PublishModeQosPolicy;
      Participant_Proxy_Reader                               : aliased RtpsReliableReaderProtocol_T;
      Participant_Proxy_Reader_Fragmentation_Resource_Limits :
      aliased DiscoveryBuiltinReaderFragmentationResourceLimits_T;
      Plugin_Promiscuity_Kind                                : aliased DiscoveryPluginPromiscuityKind;

   end record;
   pragma Convention (C, DiscoveryConfigQosPolicy);

   DISCOVERY_CONFIG_QOS_POLICY_DEFAULT : constant DiscoveryConfigQosPolicy :=
                                           (Participant_Liveliness_Lease_Duration                  => (100, 0),
                                            Participant_Liveliness_Assert_Period                   => (30, 0),
                                            Remote_Participant_Purge_Kind                          => LIVELINESS_BASED_REMOTE_PARTICIPANT_PURGE,
                                            Max_Liveliness_Loss_Detection_Period                   => (60, 0),
                                            Initial_Participant_Announcements                      => 5,
                                            Min_Initial_Participant_Announcement_Period            => (1, 0),
                                            Max_Initial_Participant_Announcement_Period            => (1, 0),
                                            Participant_Reader_Resource_Limits                     =>
                                              BUILTIN_TOPIC_READER_RESOURCE_LIMITS_DEFAULT,
                                            Publication_Reader                                     =>
                                              RTPS_RELIABLE_READER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Publication_Reader_Resource_Limits                     =>
                                              BUILTIN_TOPIC_READER_RESOURCE_LIMITS_DEFAULT,
                                            Subscription_Reader                                    =>
                                              RTPS_RELIABLE_READER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Subscription_Reader_Resource_Limits                    =>
                                              BUILTIN_TOPIC_READER_RESOURCE_LIMITS_DEFAULT,
                                            Publication_Writer                                     =>
                                              RTPS_RELIABLE_WRITER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Subscription_Writer                                    =>
                                              RTPS_RELIABLE_WRITER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Endpoint_Plugin_Redundancy_Level                       => -1,
                                            Builtin_Discovery_Plugins                              =>
                                              DISCOVERYCONFIG_BUILTIN_PLUGIN_MASK_DEFAULT,

                                             --   /***************** HIDDEN FROM USER ********************************/

                                            Publication_Reader_Fragmentation_Resource_Limits       =>
                                              DISCOVERY_BUILTIN_READER_FRAGMENTATION_RESOURCE_LIMITS_DEFAULT,
                                            Subscription_Reader_Fragmentation_Resource_Limits      =>
                                              DISCOVERY_BUILTIN_READER_FRAGMENTATION_RESOURCE_LIMITS_DEFAULT,
                                            Sedp_Rely_On_Spdp_Only                                 => FALSE,
                                            Spare1                                                 => FALSE,
                                            Spare2                                                 => FALSE,
                                            Spare3                                                 => FALSE,
                                            Inter_Participant_Reader                               =>
                                              RTPS_RELIABLE_READER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Inter_Participant_Writer                               =>
                                              RTPS_RELIABLE_WRITER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Publication_Writer_Latency_Budget                      =>
                                              LATENCY_BUDGET_QOS_POLICY_DEFAULT,
                                            Publication_Writer_Push_On_Write                       => TRUE,
                                            Spare4                                                 => FALSE,
                                            Spare5                                                 => FALSE,
                                            Spare6                                                 => FALSE,
                                            Publication_Writer_Publish_Mode                        =>
                                              PUBLISH_MODE_QOS_POLICY_DEFAULT,
                                            Subscription_Writer_Latency_Budget                     =>
                                              LATENCY_BUDGET_QOS_POLICY_DEFAULT,
                                            Subscription_Writer_Push_On_Write                      => TRUE,
                                            Spare7                                                 => FALSE,
                                            Spare8                                                 => FALSE,
                                            Spare9                                                 => FALSE,
                                            Subscription_Writer_Publish_Mode                       =>
                                              PUBLISH_MODE_QOS_POLICY_DEFAULT,
                                            Participant_State_Writer
                                                                                                   => RTPS_RELIABLE_WRITER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Participant_State_Writer_Latency_Budget                =>
                                              LATENCY_BUDGET_QOS_POLICY_DEFAULT,
                                            Participant_State_Writer_Push_On_Write                 => TRUE,
                                            Spare10                                                => FALSE,
                                            Spare11                                                => FALSE,
                                            Spare12                                                => FALSE,
                                            Participant_State_Writer_Publish_Mode                  =>
                                              PUBLISH_MODE_QOS_POLICY_DEFAULT,
                                            Participant_Proxy_Reader                               =>
                                              RTPS_RELIABLE_READER_PROTOCOL_DISCOVERY_CONFIG_DEFAULT,
                                            Participant_Proxy_Reader_Fragmentation_Resource_Limits =>
                                              DISCOVERY_BUILTIN_READER_FRAGMENTATION_RESOURCE_LIMITS_DEFAULT,
                                            Plugin_Promiscuity_Kind                                =>
                                              DISCOVERYPLUGIN_DISCOVER_MATCHING_REMOTE_ENTITIES_PROMISCUITY);

   TYPESUPPORT_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, TYPESUPPORT_QOS_POLICY_NAME, "DDS_TYPESUPPORT_QOS_POLICY_NAME");

   type TypeSupportQosPolicy is record
      Plugin_Data : aliased DDS_Support.Void_Ptr;
   end record;

   TYPESUPPORT_QOS_POLICY_DEFAULT : constant TypeSupportQosPolicy :=
                                      (Plugin_Data => System.Null_Address);

   ASYNCHRONOUSPUBLISHER_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, ASYNCHRONOUSPUBLISHER_QOS_POLICY_NAME,
                     "DDS_ASYNCHRONOUSPUBLISHER_QOS_POLICY_NAME");

   type AsynchronousPublisherQosPolicy is record
      Disable_Asynchronous_Write : aliased DDS.Boolean;
      Spare1                     : aliased DDS.Boolean;
      Spare2                     : aliased DDS.Boolean;
      Spare3                     : aliased DDS.Boolean;
      Thread                     : aliased ThreadSettings_T;
   end record;
   pragma Convention (C, AsynchronousPublisherQosPolicy);

   ASYNCHRONOUS_PUBLISHER_QOS_POLICY_DEFAULT : constant AsynchronousPublisherQosPolicy :=
                                                 (Disable_Asynchronous_Write => FALSE,
                                                  Spare1                     => FALSE,
                                                  Spare2                     => FALSE,
                                                  Spare3                     => FALSE,
                                                  Thread                     => THREAD_SETTINGS_DEFAULT);


   USEROBJECT_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, USEROBJECT_QOS_POLICY_NAME, "DDS_USEROBJECT_QOS_POLICY_NAME");

   type UserObjectQosPolicy is record
      Participant_User_Object            : aliased UserObjectSettings_T;
      Topic_User_Object                  : aliased UserObjectSettings_T;
      Content_Filtered_Topic_User_Object : aliased UserObjectSettings_T;
      Publisher_User_Object              : aliased UserObjectSettings_T;
      Data_Writer_User_Object            : aliased UserObjectSettings_T;
      Subscriber_User_Object             : aliased UserObjectSettings_T;
      Data_Reader_User_Object            : aliased UserObjectSettings_T;
      Read_Condition_User_Object         : aliased UserObjectSettings_T;
      Flow_Controller_User_Object        : aliased UserObjectSettings_T;
   end record;
   pragma Convention (C, UserObjectQosPolicy);

   USER_OBJECT_QOS_POLICY_DEFAULT : constant UserObjectQosPolicy :=
                                      (others => (0, 0));

   EXCLUSIVEAREA_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, EXCLUSIVEAREA_QOS_POLICY_NAME,
                     "DDS_EXCLUSIVEAREA_QOS_POLICY_NAME");

   type ExclusiveAreaQosPolicy is record
      Use_Shared_Exclusive_Area : aliased DDS.Boolean;
      Spare1                    : aliased DDS.Boolean;
      Spare2                    : aliased DDS.Boolean;
      Spare3                    : aliased DDS.Boolean;
   end record;
   pragma Convention (C, ExclusiveAreaQosPolicy);

   EXCLUSIVE_AREA_QOS_POLICY_DEFAULT : constant ExclusiveAreaQosPolicy :=
                                         (Use_Shared_Exclusive_Area => FALSE,
                                          others                    => FALSE);

   type Property_T is record
      Name      : aliased DDS.String;
      Value     : aliased DDS.String;
      Propagate : aliased DDS.Boolean;
      Spare1    : aliased DDS.Boolean;
      Spare2    : aliased DDS.Boolean;
      Spare3    : aliased DDS.Boolean;
   end record;
   pragma Convention (C, Property_T);

   type Property_T_Access is access all Property_T;

   type Property_T_Array is array (Natural range <>) of aliased Property_T;
   procedure Initialize (Self  : in out Property_T);
   procedure Finalize (Self  : in out Property_T);
   procedure Copy (Dst : in out Property_T; Src : in Property_T);
   package Property_T_Seq is new DDS_Support.Sequences_Generic
     (Property_T,
      Property_T_Access,
      DDS.Natural,
      1,
      Property_T_Array);

   type Property_T_Seq_Access is access all Property_T_Seq.Sequence;

   PROPERTY_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, PROPERTY_QOS_POLICY_NAME, "DDS_PROPERTY_QOS_POLICY_NAME");

   type PropertyQosPolicy is record
      Value :  aliased Property_T_Seq.Sequence;
   end record;
   pragma Convention (C, PropertyQosPolicy);

   type PropertyQosPolicy_Access is access all PropertyQosPolicy;

   PROPERTY_QOS_POLICY_DEFAULT : constant PropertyQosPolicy :=
                                   (Value => Property_T_Seq.DEFAULT_SEQUENCE);


   function PropertyQosPolicyHelper_Get_Number_Of_Properties (Policy : in PropertyQosPolicy_Access)
                                                              return Long;
   pragma Interface (C, PropertyQosPolicyHelper_Get_Number_Of_Properties,
                     "DDS_PropertyQosPolicyHelper_get_number_of_properties");


   function PropertyQosPolicyHelper_Assert_Property (Policy    : in PropertyQosPolicy_Access;
                                                     Name      : in DDS.String;
                                                     Value     : in DDS.String;
                                                     Propagate : in DDS.Boolean)
                                                     return ReturnCode_T;
   pragma Interface (C, PropertyQosPolicyHelper_Assert_Property,
                     "DDS_PropertyQosPolicyHelper_assert_property");

   function PropertyQosPolicyHelper_Add_Property (Policy    : in PropertyQosPolicy_Access;
                                                  Name      : in DDS.String;
                                                  Value     : in DDS.String;
                                                  Propagate : in DDS.Boolean)
                                                  return ReturnCode_T;
   pragma Interface (C, PropertyQosPolicyHelper_Add_Property,
                     "DDS_PropertyQosPolicyHelper_add_property");

   function PropertyQosPolicyHelper_Lookup_Property (Policy : in PropertyQosPolicy_Access;
                                                     Name   : in DDS.String)
                                                     return PropertyQosPolicy_Access;
   pragma Interface (C, PropertyQosPolicyHelper_Lookup_Property,
                     "DDS_PropertyQosPolicyHelper_lookup_property");

   function PropertyQosPolicyHelper_Remove_Property (Policy : in PropertyQosPolicy_Access;
                                                     Name   : in DDS.String)
                                                     return ReturnCode_T;
   pragma Interface (C, PropertyQosPolicyHelper_Remove_Property,
                     "DDS_PropertyQosPolicyHelper_remove_property");


   function PropertyQosPolicyHelper_Get_Properties (Policy       : in PropertyQosPolicy_Access;
                                                    Properties   : in Property_T_Seq_Access;
                                                    Name_Prefix  : in DDS.String)
                                                    return ReturnCode_T;
   pragma Interface (C, PropertyQosPolicyHelper_Get_Properties,
                     "DDS_PropertyQosPolicyHelper_get_properties");


   type ContentFilterProperty_T is record
      Content_Filter_Topic_Name : aliased DDS.String;
      Related_Topic_Name        : aliased DDS.String;
      Filter_Class_Name         : aliased DDS.String;
      Filter_Expression         : aliased DDS.String;
      Expression_Parameters     : aliased DDS.String_Seq.Sequence;
   end record;

   CONTENT_FILTER_PROPERTY_DEFAULT : constant ContentFilterProperty_T :=
                                       (Content_Filter_Topic_Name => NULL_STRING,
                                        Related_Topic_Name        => NULL_STRING,
                                        Filter_Class_Name         => NULL_STRING,
                                        Filter_Expression         => NULL_STRING,
                                        Expression_Parameters     => String_Seq.DEFAULT_SEQUENCE);

   ENTITYNAME_QOS_POLICY_NAME : constant DDS.String;
   pragma Interface (C, ENTITYNAME_QOS_POLICY_NAME, "DDS_ENTITYNAME_QOS_POLICY_NAME");

   AUTO_NAME_ENTITY  : constant DDS.String;
   pragma Interface (C, AUTO_NAME_ENTITY, "DDS_AUTO_NAME_ENTITY");

   type EntityNameQosPolicy is record
      Name : aliased DDS.String;
   end record;
   pragma Convention (C, EntityNameQosPolicy);

   ENTITY_NAME_QOS_POLICY_DEFAULT : constant EntityNameQosPolicy :=
                                      (Name => DDS.NULL_STRING);

   --
   --  From dds_c_typecode.h
   --

   --  !!! Change name back to TypeCode when typecode object removed
   type TypeCode is record
      T_Data : aliased RTI.Cdr.TypeCode;
   end record;

   type TypeCode_Access is access all TypeCode;


   --
   --  From dds_c_builtin.h
   --

   BUILTIN_TOPIC_MAX_STRING_LENGTH : constant := 256;

   type ParticipantBuiltinTopicData is record
      Key                      : aliased BuiltinTopicKey_T;
      User_Data                : aliased UserDataQosPolicy;
      --  --- Extensions: ----------------------------------------------------
      Property                 : aliased PropertyQosPolicy;
      Rtps_Protocol_Version    : aliased ProtocolVersion_T;
      Rtps_Vendor_Id           : aliased VendorId_T;
      Dds_Builtin_Endpoints    : aliased Unsigned_Long;
      Default_Unicast_Locators : aliased LocatorSeq.Sequence;
      Product_Version          : aliased ProductVersion_T;
      Plugin_Promiscuity_Kind  : aliased DiscoveryPluginPromiscuityKind;
      Participant_Name         : aliased EntityNameQosPolicy;

   end record;
   pragma Convention (C, ParticipantBuiltinTopicData);

   type ParticipantBuiltinTopicData_Access is access all ParticipantBuiltinTopicData;

   ParticipantBuiltinTopicData_INITIALIZER : constant ParticipantBuiltinTopicData :=
                                               (Key                      => BuiltinTopicKey_T_INITIALIZER,
                                                User_Data                => USER_DATA_QOS_POLICY_DEFAULT,
                                                --  --- Extensions: ----------------------------------------------------
                                                Property                 => PROPERTY_QOS_POLICY_DEFAULT,
                                                Rtps_Protocol_Version    => PROTOCOL_VERSION_DEFAULT,
                                                Rtps_Vendor_Id           => VENDOR_ID_DEFAULT,
                                                Dds_Builtin_Endpoints    => 0,
                                                Default_Unicast_Locators => LocatorSeq.DEFAULT_SEQUENCE,
                                                Product_Version          => PRODUCTVERSION_UNKNOWN,
                                                Plugin_Promiscuity_Kind  => DISCOVERYPLUGIN_DISCOVER_MATCHING_REMOTE_ENTITIES_PROMISCUITY,
                                                Participant_Name         => ENTITY_NAME_QOS_POLICY_DEFAULT);

   procedure ParticipantBuiltinTopicDataPlugin_Print (UserData : ParticipantBuiltinTopicData_Access;
                                                      Desc     : DDS.String;
                                                      Indent   : Long);
   pragma Interface (C,  ParticipantBuiltinTopicDataPlugin_Print,
                     "DDS_ParticipantBuiltinTopicDataPlugin_print");

   type TopicBuiltinTopicData is record
      Key                       : aliased BuiltinTopicKey_T;
      Name                      : aliased DDS.String;
      Type_Name                 : aliased DDS.String;
      Durability                : aliased DurabilityQosPolicy;
      Durability_Service        : aliased DurabilityServiceQosPolicy;
      Deadline                  : aliased DeadlineQosPolicy;
      Latency_Budget            : aliased LatencyBudgetQosPolicy;
      Liveliness                : aliased LivelinessQosPolicy;
      Reliability               : aliased ReliabilityQosPolicy;
      Transport_Priority        : aliased TransportPriorityQosPolicy;
      Lifespan                  : aliased LifespanQosPolicy;
      Destination_Order         : aliased DestinationOrderQosPolicy;
      History                   : aliased HistoryQosPolicy;
      Resource_Limits           : aliased ResourceLimitsQosPolicy;
      Ownership                 : aliased OwnershipQosPolicy;
      Topic_Data                : aliased TopicDataQosPolicy;
   end record;
   pragma Convention (C, TopicBuiltinTopicData);

   type TopicBuiltinTopicData_Access is access all TopicBuiltinTopicData;

   TopicBuiltinTopicData_INITIALIZER : constant TopicBuiltinTopicData :=
                                         (Key                        => BuiltinTopicKey_T_INITIALIZER,
                                          Name                       => NULL_STRING,
                                          Type_Name                  => NULL_STRING,
                                          Durability                 => DURABILITY_QOS_POLICY_DEFAULT,
                                          Durability_Service         => DURABILITY_SERVICE_QOS_POLICY_DEFAULT,
                                          Deadline                   => DEADLINE_QOS_POLICY_DEFAULT,
                                          Latency_Budget             => LATENCY_BUDGET_QOS_POLICY_DEFAULT,
                                          Liveliness                 => LIVELINESS_QOS_POLICY_DEFAULT,
                                          Reliability                => RELIABILITY_QOS_POLICY_DEFAULT,
                                          Transport_Priority         => TRANSPORT_PRIORITY_QOS_POLICY_DEFAULT,
                                          Lifespan                   => LIFESPAN_QOS_POLICY_DEFAULT,
                                          Destination_Order          => DESTINATION_ORDER_QOS_POLICY_DEFAULT,
                                          History                    => HISTORY_QOS_POLICY_DEFAULT,
                                          Resource_Limits            => RESOURCE_LIMITS_QOS_POLICY_DEFAULT,
                                          Ownership                  => OWNERSHIP_QOS_POLICY_DEFAULT,
                                          Topic_Data                 => TOPIC_DATA_QOS_POLICY_DEFAULT);

   type PublicationBuiltinTopicData is record
      Key                       : aliased BuiltinTopicKey_T;
      Participant_Key           : aliased BuiltinTopicKey_T;
      Topic_Name                : aliased DDS.String;
      Type_Name                 : aliased DDS.String;
      Max_Sample_Serialize_Size : aliased Long;
      Durability                : aliased DurabilityQosPolicy;
      Durability_Service        : aliased DurabilityServiceQosPolicy;
      Deadline                  : aliased DeadlineQosPolicy;
      Latency_Budget            : aliased LatencyBudgetQosPolicy;
      Liveliness                : aliased LivelinessQosPolicy;
      Reliability               : aliased ReliabilityQosPolicy;
      Lifespan                  : aliased LifespanQosPolicy;
      User_Data                 : aliased UserDataQosPolicy;
      Ownership                 : aliased OwnershipQosPolicy;
      Ownership_Strength        : aliased OwnershipStrengthQosPolicy;
      Destination_Order         : aliased DestinationOrderQosPolicy;
      Presentation              : aliased PresentationQosPolicy;
      Partition                 : aliased PartitionQosPolicy;
      Topic_Data                : aliased TopicDataQosPolicy;
      Group_Data                : aliased GroupDataQosPolicy;
      --  extensions
      Type_Code                 : aliased TypeCode_Access;
      Publisher_Key             : aliased BuiltinTopicKey_T;
      Property                  : aliased PropertyQosPolicy;
      Unicast_Locators          : aliased LocatorSeq.Sequence;
      Virtual_Guid              : aliased Guid_T;
      Service                   : aliased ServiceQosPolicy;
      Rtps_Protocol_Version     : aliased ProtocolVersion_T;
      Rtps_Vendor_Id            : aliased VendorId_T;
      Product_Version           : aliased ProductVersion_T;
   end record;
   pragma Convention (C, PublicationBuiltinTopicData);

   type PublicationBuiltinTopicData_Access is access all PublicationBuiltinTopicData;

   PublicationBuiltinTopicData_INITIALIZER : constant PublicationBuiltinTopicData :=
                                               (Key                        => BuiltinTopicKey_T_INITIALIZER,
                                                Participant_Key            => BuiltinTopicKey_T_INITIALIZER,
                                                Topic_Name                 => NULL_STRING,
                                                Type_Name                  => NULL_STRING,
                                                Max_Sample_Serialize_Size  => 0,
                                                Durability                 => DURABILITY_QOS_POLICY_DEFAULT,
                                                Durability_Service         => DURABILITY_SERVICE_QOS_POLICY_DEFAULT,
                                                Deadline                   => DEADLINE_QOS_POLICY_DEFAULT,
                                                Latency_Budget             => LATENCY_BUDGET_QOS_POLICY_DEFAULT,
                                                Liveliness                 => LIVELINESS_QOS_POLICY_DEFAULT,
                                                Reliability                => RELIABILITY_QOS_POLICY_DEFAULT,
                                                Lifespan                   => LIFESPAN_QOS_POLICY_DEFAULT,
                                                User_Data                  => USER_DATA_QOS_POLICY_DEFAULT,
                                                Ownership                  => OWNERSHIP_QOS_POLICY_DEFAULT,
                                                Ownership_Strength         => OWNERSHIP_STRENGTH_QOS_POLICY_DEFAULT,
                                                Destination_Order          => DESTINATION_ORDER_QOS_POLICY_DEFAULT,
                                                Presentation               => PRESENTATION_QOS_POLICY_DEFAULT,
                                                Partition                  => PARTITION_QOS_POLICY_DEFAULT,
                                                Topic_Data                 => TOPIC_DATA_QOS_POLICY_DEFAULT,
                                                Group_Data                 => GROUP_DATA_QOS_POLICY_DEFAULT,
                                                Type_Code                  => null,
                                                Publisher_Key              => BuiltinTopicKey_T_INITIALIZER,
                                                Property                   => PROPERTY_QOS_POLICY_DEFAULT,
                                                Unicast_Locators           => LocatorSeq.DEFAULT_SEQUENCE,
                                                Virtual_Guid               => (Value => (others => 0)),
                                                Service                    => SERVICE_QOS_POLICY_DEFAULT,
                                                Rtps_Protocol_Version      => PROTOCOL_VERSION_DEFAULT,
                                                Rtps_Vendor_Id             => VENDOR_ID_DEFAULT,
                                                Product_Version            => PRODUCTVERSION_UNKNOWN);

   type SubscriptionBuiltinTopicData is record
      Key                     : aliased BuiltinTopicKey_T;
      Participant_Key         : aliased BuiltinTopicKey_T;
      Topic_Name              : aliased DDS.String;
      Type_Name               : aliased DDS.String;
      Durability              : aliased DurabilityQosPolicy;
      Deadline                : aliased DeadlineQosPolicy;
      Latency_Budget          : aliased LatencyBudgetQosPolicy;
      Liveliness              : aliased LivelinessQosPolicy;
      Reliability             : aliased ReliabilityQosPolicy;
      Ownership               : aliased OwnershipQosPolicy;
      Destination_Order       : aliased DestinationOrderQosPolicy;
      User_Data               : aliased UserDataQosPolicy;
      Time_Based_Filter       : aliased TimeBasedFilterQosPolicy;
      Presentation            : aliased PresentationQosPolicy;
      Partition               : aliased PartitionQosPolicy;
      Topic_Data              : aliased TopicDataQosPolicy;
      Group_Data              : aliased GroupDataQosPolicy;
      --  extensions
      Type_Code               : aliased TypeCode_Access;
      Subscriber_Key          : aliased BuiltinTopicKey_T;
      Property                : aliased PropertyQosPolicy;
      Unicast_Locators        : aliased LocatorSeq.Sequence;
      Multicast_Locators      : aliased LocatorSeq.Sequence;
      Content_Filter_Property : aliased ContentFilterProperty_T;
      Virtual_Guid            : aliased Guid_T;
      Service                 : aliased ServiceQosPolicy;
      Rtps_Protocol_Version   : aliased ProtocolVersion_T;
      Rtps_Vendor_Id          : aliased VendorId_T;
      Product_Version         : aliased ProductVersion_T;
   end record;
   pragma Convention (C, SubscriptionBuiltinTopicData);

   type SubscriptionBuiltinTopicData_Access is access all SubscriptionBuiltinTopicData;

   SubscriptionBuiltinTopicData_INITIALIZER : constant SubscriptionBuiltinTopicData :=
                                                (Key                     => BuiltinTopicKey_T_INITIALIZER,
                                                 Participant_Key         => BuiltinTopicKey_T_INITIALIZER,
                                                 Topic_Name              => NULL_STRING,
                                                 Type_Name               => NULL_STRING,
                                                 Durability              => DURABILITY_QOS_POLICY_DEFAULT,
                                                 Deadline                => DEADLINE_QOS_POLICY_DEFAULT,
                                                 Latency_Budget          => LATENCY_BUDGET_QOS_POLICY_DEFAULT,
                                                 Liveliness              => LIVELINESS_QOS_POLICY_DEFAULT,
                                                 Reliability             => RELIABILITY_QOS_POLICY_DEFAULT,
                                                 Ownership               => OWNERSHIP_QOS_POLICY_DEFAULT,
                                                 Destination_Order       => DESTINATION_ORDER_QOS_POLICY_DEFAULT,
                                                 User_Data               => USER_DATA_QOS_POLICY_DEFAULT,
                                                 Time_Based_Filter       => TIME_BASED_FILTER_QOS_POLICY_DEFAULT,
                                                 Presentation            => PRESENTATION_QOS_POLICY_DEFAULT,
                                                 Partition               => PARTITION_QOS_POLICY_DEFAULT,
                                                 Topic_Data              => TOPIC_DATA_QOS_POLICY_DEFAULT,
                                                 Group_Data              => GROUP_DATA_QOS_POLICY_DEFAULT,
                                                --  extensions
                                                 Type_Code               => null,
                                                 Subscriber_Key          => BuiltinTopicKey_T_INITIALIZER,
                                                 Property                => PROPERTY_QOS_POLICY_DEFAULT,
                                                 Unicast_Locators        => LocatorSeq.DEFAULT_SEQUENCE,
                                                 Multicast_Locators      => LocatorSeq.DEFAULT_SEQUENCE,
                                                 Content_Filter_Property => CONTENT_FILTER_PROPERTY_DEFAULT,
                                                 Virtual_Guid            =>  (Value => (others => 0)),
                                                 Service                 => SERVICE_QOS_POLICY_DEFAULT,
                                                 Rtps_Protocol_Version   => PROTOCOL_VERSION_DEFAULT,
                                                 Rtps_Vendor_Id          => VENDOR_ID_DEFAULT,
                                                 Product_Version         => PRODUCTVERSION_UNKNOWN);

   --
   --  dds/dds_c_topic.h
   --

   type InconsistentTopicStatus is record
      Total_Count        : aliased Long;
      Total_Count_Change : aliased Long;
   end record;
   pragma Convention (C, InconsistentTopicStatus);

   InconsistentTopicStatus_INITIALIZER : constant InconsistentTopicStatus :=
                                           (Total_Count        => 0,
                                            Total_Count_Change => 0);

   type TopicQos is new Ada.Finalization.Limited_Controlled with record
      Topic_Data         : aliased TopicDataQosPolicy;
      Durability         : aliased DurabilityQosPolicy;
      Durability_Service : aliased DurabilityServiceQosPolicy;
      Deadline           : aliased DeadlineQosPolicy;
      Latency_Budget     : aliased LatencyBudgetQosPolicy;
      Liveliness         : aliased LivelinessQosPolicy;
      Reliability        : aliased ReliabilityQosPolicy;
      Destination_Order  : aliased DestinationOrderQosPolicy;
      History            : aliased HistoryQosPolicy;
      Resource_Limits    : aliased ResourceLimitsQosPolicy;
      Transport_Priority : aliased TransportPriorityQosPolicy;
      Lifespan           : aliased LifespanQosPolicy;
      Ownership          : aliased OwnershipQosPolicy;
   end record;
   pragma Convention (C, TopicQos);

   procedure Initialize (Self : in out TopicQos);
   procedure Finalize (Self : in out TopicQos);
   procedure Copy (Dst : in out TopicQos;
                   Src : in out TopicQos);
   --
   --  dds/dds_c_publication.h
   --

   type OfferedDeadlineMissedStatus is record
      Total_Count          : aliased Long;
      Total_Count_Change   : aliased Long;
      Last_Instance_Handle : aliased InstanceHandle_T;
   end record;
   pragma Convention (C, OfferedDeadlineMissedStatus);

   OfferedDeadlineMissedStatus_INITIALIZER : constant OfferedDeadlineMissedStatus :=
                                               (Total_Count          => 0,
                                                Total_Count_Change   => 0,
                                                Last_Instance_Handle => Null_InstanceHandle_T);

   type LivelinessLostStatus is record
      Total_Count        : aliased Long;
      Total_Count_Change : aliased Long;
   end record;
   pragma Convention (C, LivelinessLostStatus);

   LivelinessLostStatus_INITIALIZER : constant LivelinessLostStatus :=
                                        (Total_Count        => 0,
                                         Total_Count_Change => 0);

   type OfferedIncompatibleQosStatus is record
      Total_Count        : aliased Long;
      Total_Count_Change : aliased Long;
      Last_Policy_Id     : aliased QosPolicyId_T;
      Policies           : aliased QosPolicyCount_Seq.Sequence;

   end record;
   pragma Convention (C, OfferedIncompatibleQosStatus);

   OfferedIncompatibleQosStatus_INITIALIZER : constant OfferedIncompatibleQosStatus :=
                                                (Total_Count        => 0,
                                                 Total_Count_Change => 0,
                                                 Last_Policy_Id     => INVALID_QOS_POLICY_ID,
                                                 Policies           => QosPolicyCount_Seq.DEFAULT_SEQUENCE);

   type PublicationMatchedStatus is record
      Total_Count              : aliased Long;
      Total_Count_Change       : aliased Long;
      Current_Count            : aliased Long;
      Current_Count_Change     : aliased Long;
      Last_Subscription_Handle : aliased InstanceHandle_T;
   end record;
   pragma Convention (C, PublicationMatchedStatus);

   PublicationMatchedStatus_INITIALIZER : constant PublicationMatchedStatus :=
                                            (Total_Count              => 0,
                                             Total_Count_Change       => 0,
                                             Current_Count            => 0,
                                             Current_Count_Change     => 0,
                                             Last_Subscription_Handle => Null_InstanceHandle_T);

   type ReliableWriterCacheEventCount is record
      Total_Count        : aliased Long;
      Total_Count_Change : aliased Long;
   end record;
   pragma Convention (C, ReliableWriterCacheEventCount);

   ReliableWriterCacheEventCount_INITIALIZER : constant ReliableWriterCacheEventCount :=
                                                 (Total_Count        => 0,
                                                  Total_Count_Change => 0);

   type ReliableWriterCacheChangedStatus is record
      Empty_Reliable_Writer_Cache          : aliased ReliableWriterCacheEventCount;
      Full_Reliable_Writer_Cache           : aliased ReliableWriterCacheEventCount;
      Low_Watermark_Reliable_Writer_Cache  : aliased ReliableWriterCacheEventCount;
      High_Watermark_Reliable_Writer_Cache : aliased ReliableWriterCacheEventCount;
      Unacknowledged_Sample_Count          : aliased Long;
   end record;
   pragma Convention (C, ReliableWriterCacheChangedStatus);

   ReliableWriterCacheChangedStatus_INITIALIZER : constant ReliableWriterCacheChangedStatus :=
                                                    (Empty_Reliable_Writer_Cache          => ReliableWriterCacheEventCount_INITIALIZER,
                                                     Full_Reliable_Writer_Cache           => ReliableWriterCacheEventCount_INITIALIZER,
                                                     Low_Watermark_Reliable_Writer_Cache  => ReliableWriterCacheEventCount_INITIALIZER,
                                                     High_Watermark_Reliable_Writer_Cache => ReliableWriterCacheEventCount_INITIALIZER,
                                                     Unacknowledged_Sample_Count          => 0);

   type ReliableReaderActivityChangedStatus is record
      Active_Count          : aliased Long;
      Inactive_Count        : aliased Long;
      Active_Count_Change   : aliased Long;
      Inactive_Count_Change : aliased Long;
      Last_Instance_Handle  : aliased InstanceHandle_T;
   end record;
   pragma Convention (C, ReliableReaderActivityChangedStatus);

   ReliableReaderActivityChangedStatus_INITIALIZER : constant  ReliableReaderActivityChangedStatus :=
                                                       (Active_Count          => 0,
                                                        Inactive_Count        => 0,
                                                        Active_Count_Change   => 0,
                                                        Inactive_Count_Change => 0,
                                                        Last_Instance_Handle  => Null_InstanceHandle_T);

   type DataWriterQos is new Ada.Finalization.Limited_Controlled with record
   ---
      Durability             : aliased DurabilityQosPolicy;
      Durability_Service     : aliased DurabilityServiceQosPolicy;
      Deadline               : aliased DeadlineQosPolicy;
      Latency_Budget         : aliased LatencyBudgetQosPolicy;
      Liveliness             : aliased LivelinessQosPolicy;
      Reliability            : aliased ReliabilityQosPolicy;
      Destination_Order      : aliased DestinationOrderQosPolicy;
      History                : aliased HistoryQosPolicy;
      Resource_Limits        : aliased ResourceLimitsQosPolicy;
      Transport_Priority     : aliased TransportPriorityQosPolicy;
      Lifespan               : aliased LifespanQosPolicy;
      User_Data              : aliased UserDataQosPolicy;
      Ownership              : aliased OwnershipQosPolicy;
      Ownership_Strength     : aliased OwnershipStrengthQosPolicy;
      Writer_Data_Lifecycle  : aliased WriterDataLifecycleQosPolicy;
      -- --- Extensions: ----------------------------------------------------
      Writer_Resource_Limits : aliased DataWriterResourceLimitsQosPolicy;
      Protocol               : aliased DataWriterProtocolQosPolicy;
      Transport_Selection    : aliased TransportSelectionQosPolicy;
      Unicast                : aliased TransportUnicastQosPolicy;
      Type_Support           : aliased TypeSupportQosPolicy;
      Publish_Mode           : aliased PublishModeQosPolicy;
      Property               : aliased PropertyQosPolicy;
      Service                : aliased ServiceQosPolicy;
   end record;
   pragma Convention (C, DataWriterQos);

   procedure Initialize (Self : in out DataWriterQos);
   procedure Finalize (Self : in out DataWriterQos);
   procedure Copy (Dst : in out DataWriterQos;
                   Src : in out DataWriterQos);

   type PublisherQos is new Ada.Finalization.Limited_Controlled with record
      Presentation           : PresentationQosPolicy;
      Partition              : PartitionQosPolicy;
      Group_Data             : GroupDataQosPolicy;
      Entity_Factory         : EntityFactoryQosPolicy;
      -- --- Extensions: ----------------------------------------------------
      Asynchronous_Publisher : aliased AsynchronousPublisherQosPolicy;
      Exclusive_Area         : aliased ExclusiveAreaQosPolicy;
   end record;
   pragma Convention (C, PublisherQos);

   procedure Initialize (Self : in out PublisherQos);
   procedure Finalize (Self : in out PublisherQos);
   procedure Copy (Dst : in out PublisherQos;
                   Src : in out PublisherQos);

   type DataWriterLocalWriterStatistics is record
      Queue_Level_Peak          : aliased Unsigned_Long;
      Matched_Reader_Count_Peak : aliased Unsigned_Long;
      Samples_Sent_Count        : aliased Unsigned_Long;
      Samples_Sent_Bytes        : aliased Unsigned_Long;
      Samples_Filtered_Count    : aliased Unsigned_Long;
      Heartbeats_Sent_Count     : aliased Unsigned_Long;
      Heartbeats_Sent_Bytes     : aliased Unsigned_Long;
      Samples_Resent_Count      : aliased Unsigned_Long;
      Samples_Resent_Bytes      : aliased Unsigned_Long;
      Acks_Received_Count       : aliased Unsigned_Long;
      Acks_Received_Bytes       : aliased Unsigned_Long;
      Nacks_Received_Count      : aliased Unsigned_Long;
      Nacks_Received_Bytes      : aliased Unsigned_Long;
      Gaps_Sent_Count           : aliased Unsigned_Long;
      Gaps_Sent_Bytes           : aliased Unsigned_Long;
      Samples_Rejected_Count    : aliased Unsigned_Long;
   end record;
   pragma Convention (C, DataWriterLocalWriterStatistics);

   DataWriterLocalWriterStatistics_INITIALIZER : constant DataWriterLocalWriterStatistics :=
                                                   (others => 0);

   type DataWriterMatchedReaderStatistics is record
      Samples_Sent_Count     : aliased Unsigned_Long;
      Samples_Sent_Bytes     : aliased Unsigned_Long;
      Samples_Filtered_Count : aliased Unsigned_Long;
      Heartbeats_Sent_Count  : aliased Unsigned_Long;
      Heartbeats_Sent_Bytes  : aliased Unsigned_Long;
      Samples_Resent_Count   : aliased Unsigned_Long;
      Samples_Resent_Bytes   : aliased Unsigned_Long;
      Acks_Received_Count    : aliased Unsigned_Long;
      Acks_Received_Bytes    : aliased Unsigned_Long;
      Nacks_Received_Count   : aliased Unsigned_Long;
      Nacks_Received_Bytes   : aliased Unsigned_Long;
      Gaps_Sent_Count        : aliased Unsigned_Long;
      Gaps_Sent_Bytes        : aliased Unsigned_Long;
   end record;
   pragma Convention (C, DataWriterMatchedReaderStatistics);

   DataWriterMatchedReadersStatistics_INITIALIZER :
   constant DataWriterMatchedReaderStatistics :=
                                                      (others => 0);

   type DataWriterMatchedDestinationStatistics is record
      Samples_Sent_Count     : aliased Unsigned_Long;
      Samples_Sent_Bytes     : aliased Unsigned_Long;
      Samples_Filtered_Count : aliased Unsigned_Long;
      Heartbeats_Sent_Count  : aliased Unsigned_Long;
      Heartbeats_Sent_Bytes  : aliased Unsigned_Long;
      Samples_Resent_Count   : aliased Unsigned_Long;
      Samples_Resent_Bytes   : aliased Unsigned_Long;
      Gaps_Sent_Count        : aliased Unsigned_Long;
      Gaps_Sent_Bytes        : aliased Unsigned_Long;
   end record;
   pragma Convention (C, DataWriterMatchedDestinationStatistics);

   DataWriterMatchedDestinationStatistics_INITIALIZER :
   constant DataWriterMatchedDestinationStatistics := (others => 0);

   --
   --  dds/dds_c_subscription.h
   --
   type RequestedDeadlineMissedStatus is record
      Total_Count          : aliased Long;
      Total_Count_Change   : aliased Long;
      Last_Instance_Handle : aliased InstanceHandle_T;
   end record;
   pragma Convention (C, RequestedDeadlineMissedStatus);

   RequestedDeadlineMissedStatus_INITIALIZER : constant RequestedDeadlineMissedStatus :=
                                                 (Total_Count              => 0,
                                                  Total_Count_Change       => 0,
                                                  Last_Instance_Handle     => Null_InstanceHandle_T);

   type LivelinessChangedStatus is record
      Alive_Count             : aliased Long;
      Not_Alive_Count         : aliased Long;
      Alive_Count_Change      : aliased Long;
      Not_Alive_Count_Change  : aliased Long;
      Last_Publication_Handle : aliased InstanceHandle_T;
   end record;
   pragma Convention (C, LivelinessChangedStatus);

   LivelinessChangedStatus_INITIALIZER : constant LivelinessChangedStatus :=
                                           (Alive_Count             => 0,
                                            Not_Alive_Count         => 0,
                                            Alive_Count_Change      => 0,
                                            Not_Alive_Count_Change  => 0,
                                            Last_Publication_Handle => Null_InstanceHandle_T);

   type RequestedIncompatibleQosStatus is record
      Total_Count        : aliased Long;
      Total_Count_Change : aliased Long;
      Last_Policy_Id     : aliased QosPolicyId_T;
      Policies           : aliased QosPolicyCount_Seq.Sequence;
   end record;
   pragma Convention (C, RequestedIncompatibleQosStatus);

   RequestedIncompatibleQosStatus_INITIALIZER : constant RequestedIncompatibleQosStatus :=
                                                  (Total_Count        => 0,
                                                   Total_Count_Change => 0,
                                                   Last_Policy_Id     => INVALID_QOS_POLICY_ID,
                                                   Policies           => QosPolicyCount_Seq.DEFAULT_SEQUENCE);

   type SampleLostStatus is record
      Total_Count        : aliased Long;
      Total_Count_Change : aliased Long;
   end record;
   pragma Convention (C, SampleLostStatus);

   SampleLostStatus_INITIALIZER : constant SampleLostStatus :=
                                    (Total_Count        => 0,
                                     Total_Count_Change => 0);

   type SampleRejectedStatusKind is
     (NOT_REJECTED,
      REJECTED_BY_INSTANCES_LIMIT,
      REJECTED_BY_SAMPLES_LIMIT,
      REJECTED_BY_SAMPLES_PER_INSTANCE_LIMIT,
      REJECTED_BY_REMOTE_WRITERS_LIMIT,
      REJECTED_BY_REMOTE_WRITERS_PER_INSTANCE_LIMIT,
      REJECTED_BY_SAMPLES_PER_REMOTE_WRITER_LIMIT);
   pragma Convention (C, SampleRejectedStatusKind);

   type SampleRejectedStatus is record
      Total_Count          : aliased Long;
      Total_Count_Change   : aliased Long;
      Last_Reason          : aliased SampleRejectedStatusKind;
      Last_Instance_Handle : aliased InstanceHandle_T;
   end record;
   pragma Convention (C, SampleRejectedStatus);

   SampleRejectedStatus_INITIALIZER : constant SampleRejectedStatus :=
                                        (Total_Count          => 0,
                                         Total_Count_Change   => 0,
                                         Last_Reason          => NOT_REJECTED,
                                         Last_Instance_Handle => Null_InstanceHandle_T);


   type SubscriptionMatchedStatus is record
      Total_Count             : aliased Long;
      Total_Count_Change      : aliased Long;
      Current_Count           : aliased Long;
      Current_Count_Change    : aliased Long;
      Last_Publication_Handle : aliased InstanceHandle_T;
   end record;
   pragma Convention (C, SubscriptionMatchedStatus);

   SubscriptionMatchedStatus_INITIALIZER : constant SubscriptionMatchedStatus :=
                                             (Total_Count              => 0,
                                              Total_Count_Change       => 0,
                                              Current_Count            => 0,
                                              Current_Count_Change     => 0,
                                              Last_Publication_Handle  => Null_InstanceHandle_T);

   type SampleStateKind is new Unsigned_Long;
   READ_SAMPLE_STATE : constant SampleStateKind     := 1;
   NOT_READ_SAMPLE_STATE : constant SampleStateKind := 2;

   subtype SampleStateMask is SampleStateKind;
   ANY_SAMPLE_STATE : constant SampleStateMask := 65535;

   type SampleStateKind_Access is access constant SampleStateKind;

   type ViewStateKind is new Unsigned_Long;
   NEW_VIEW_STATE : constant ViewStateKind     := 1;
   NOT_NEW_VIEW_STATE : constant ViewStateKind := 2;

   subtype ViewStateMask is ViewStateKind;
   ANY_VIEW_STATE : constant ViewStateMask      := 65535;

   type ViewStateKind_Access is access constant ViewStateKind;

   type InstanceStateKind is   new Unsigned_Long;
   ALIVE_INSTANCE_STATE : constant InstanceStateKind                := 1;
   NOT_ALIVE_DISPOSED_INSTANCE_STATE : constant InstanceStateKind   := 2;
   NOT_ALIVE_NO_WRITERS_INSTANCE_STATE : constant InstanceStateKind := 4;

   subtype InstanceStateMask is InstanceStateKind;
   NOT_ALIVE_INSTANCE_STATE : constant InstanceStateMask := 6;
   ANY_INSTANCE_STATE : constant InstanceStateMask := 65535;

   type InstanceStateKind_Access is access constant InstanceStateKind;

   type SampleInfo is record
      Sample_State                        : aliased SampleStateKind;
      View_State                          : aliased ViewStateKind;
      Instance_State                      : aliased InstanceStateKind;
      Source_Timestamp                    : aliased Time_T;
      Instance_Handle                     : aliased InstanceHandle_T;
      Publication_Handle                  : aliased InstanceHandle_T;
      Disposed_Generation_Count           : aliased Long;
      No_Writers_Generation_Count         : aliased Long;
      Sample_Rank                         : aliased Long;
      Generation_Rank                     : aliased Long;
      Absolute_Generation_Rank            : aliased Long;
      Valid_Data                          : aliased DDS.Boolean;
      Spare1                              : aliased DDS.Boolean;
      Spare2                              : aliased DDS.Boolean;
      Spare3                              : aliased DDS.Boolean;
      Reception_Timestamp                 : aliased Time_T;
      Publication_Sequence_Number         : aliased SequenceNumber_T;
      Reception_Sequence_Number           : aliased SequenceNumber_T;
      Publication_Virtual_Guid            : aliased Guid_T;
      Publication_Virtual_Sequence_Number : aliased SequenceNumber_T;
   end record;
   pragma Convention (C, SampleInfo);

   type SampleInfo_Access is access all SampleInfo;
   type SampleInfo_Array is array (Natural range <>) of aliased SampleInfo;
   procedure Initialize (Self  : in out SampleInfo);
   procedure Finalize (Self  : in out SampleInfo);
   procedure Copy (Dst : in out SampleInfo; Src : in SampleInfo);

   package SampleInfo_Seq is new DDS_Support.Sequences_Generic
     (SampleInfo,
      SampleInfo_Access,
      DDS.Natural,
      1,
      SampleInfo_Array);


   type DataReaderQos is new Ada.Finalization.Limited_Controlled with record
   ---
      Durability             : aliased DurabilityQosPolicy;
      Deadline               : aliased DeadlineQosPolicy;
      Latency_Budget         : aliased LatencyBudgetQosPolicy;
      Liveliness             : aliased LivelinessQosPolicy;
      Reliability            : aliased ReliabilityQosPolicy;
      Destination_Order      : aliased DestinationOrderQosPolicy;
      History                : aliased HistoryQosPolicy;
      Resource_Limits        : aliased ResourceLimitsQosPolicy;
      User_Data              : aliased UserDataQosPolicy;
      Ownership              : aliased OwnershipQosPolicy;
      Time_Based_Filter      : aliased TimeBasedFilterQosPolicy;
      Reader_Data_Lifecycle  : aliased ReaderDataLifecycleQosPolicy;
      Reader_Resource_Limits : aliased DataReaderResourceLimitsQosPolicy;
      Protocol               : aliased DataReaderProtocolQosPolicy;
      Transport_Selection    : aliased TransportSelectionQosPolicy;
      Unicast                : aliased TransportUnicastQosPolicy;
      Multicast              : aliased TransportMulticastQosPolicy;
      Type_Support           : aliased TypeSupportQosPolicy;
      Property               : aliased PropertyQosPolicy;
      Service                : aliased ServiceQosPolicy;

   end record;
   pragma Convention (C, DataReaderQos);

   procedure Initialize (Self : in out DataReaderQos);
   procedure Finalize (Self : in out DataReaderQos);
   procedure Copy (Dst : in out DataReaderQos;
                   Src : in out DataReaderQos);

   type SubscriberQos is new Ada.Finalization.Limited_Controlled with record
      Presentation   : PresentationQosPolicy;
      Partition      : PartitionQosPolicy;
      Group_Data     : GroupDataQosPolicy;
      Entity_Factory : EntityFactoryQosPolicy;
      -- --- Extensions: ----------------------------------------------------
      Exclusive_Area : ExclusiveAreaQosPolicy;
   end record;
   pragma Convention (C, SubscriberQos);

   procedure Initialize (Self : in out SubscriberQos);
   procedure Finalize (Self : in out SubscriberQos);
   procedure Copy (Dst : in out SubscriberQos;
                   Src : in out SubscriberQos);

   type DataReaderLocalReaderStatistics is record
      Queue_Level_Peak           : aliased Unsigned_Long;
      Matched_Writers_Count_Peak : aliased Unsigned_Long;
      New_Samples_Received_Count : aliased Unsigned_Long;
      New_Samples_Received_Bytes : aliased Unsigned_Long;
      Old_Samples_Received_Count : aliased Unsigned_Long;
      Old_Samples_Received_Bytes : aliased Unsigned_Long;
      Samples_Filtered_Count     : aliased Unsigned_Long;
      Heartbeats_Received_Count  : aliased Unsigned_Long;
      Heartbeats_Received_Bytes  : aliased Unsigned_Long;
      Acks_Sent_Count            : aliased Unsigned_Long;
      Acks_Sent_Bytes            : aliased Unsigned_Long;
      Nacks_Sent_Count           : aliased Unsigned_Long;
      Nacks_Sent_Bytes           : aliased Unsigned_Long;
      Gaps_Received_Count        : aliased Unsigned_Long;
      Gaps_Received_Bytes        : aliased Unsigned_Long;
      Samples_Rejected_Count     : aliased Unsigned_Long;
   end record;
   pragma Convention (C, DataReaderLocalReaderStatistics);

   DataReaderLocalReaderStatistics_INITIALIZER : constant DataReaderLocalReaderStatistics :=
                                                   (others => 0);

   type DataReaderMatchedWriterStatistics is record
      New_Samples_Received_Count : aliased Unsigned_Long;
      New_Samples_Received_Bytes : aliased Unsigned_Long;
      Old_Samples_Received_Count : aliased Unsigned_Long;
      Old_Samples_Received_Bytes : aliased Unsigned_Long;
      Samples_Filtered_Count     : aliased Unsigned_Long;
      Heartbeats_Received_Count  : aliased Unsigned_Long;
      Heartbeats_Received_Bytes  : aliased Unsigned_Long;
      Acks_Sent_Count            : aliased Unsigned_Long;
      Acks_Sent_Bytes            : aliased Unsigned_Long;
      Nacks_Sent_Count           : aliased Unsigned_Long;
      Nacks_Sent_Bytes           : aliased Unsigned_Long;
      Gaps_Received_Count        : aliased Unsigned_Long;
      Gaps_Received_Bytes        : aliased Unsigned_Long;
      Samples_Rejected_Count     : aliased Unsigned_Long;
   end record;

   DataReaderMatchedWriterStatistics_INITIALIZER : constant DataReaderMatchedWriterStatistics :=
                                                     (others => 0);


   --
   --  From dds_c_domain.h
   --

   type DomainParticipantQos is new Ada.Finalization.Limited_Controlled with record
      User_Data         : aliased UserDataQosPolicy;
      Entity_Factory    : aliased EntityFactoryQosPolicy;
      -- --------------- Extensions: ----------------------------------
      Wire_Protocol     : aliased WireProtocolQosPolicy;
      Transport_Builtin : aliased TransportBuiltinQosPolicy;
      Default_Unicast   : aliased TransportUnicastQosPolicy;
      Discovery         : aliased DiscoveryQosPolicy;
      Resource_Limits   : aliased DomainParticipantResourceLimitsQosPolicy;
      Event             : aliased EventQosPolicy;
      Receiver_Pool     : aliased ReceiverPoolQosPolicy;
      Database          : aliased DatabaseQosPolicy;
      Discovery_Config  : aliased DiscoveryConfigQosPolicy;
      Exclusive_Area    : aliased ExclusiveAreaQosPolicy;
      Property          : aliased PropertyQosPolicy;
      User_Object       : aliased UserObjectQosPolicy;
      Statistics        : aliased StatisticsQosPolicy;
      Participant_Name  : aliased EntityNameQosPolicy;
   end record;
   pragma Convention (C, DomainParticipantQos);

   procedure Initialize (Self : in out DomainParticipantQos);
   procedure Finalize (Self : in out DomainParticipantQos);
   procedure Copy (Dst : in out DomainParticipantQos;
                   Src : in DomainParticipantQos);

   type DomainId_T is new Long;

   type DomainParticipantFactoryQos is record
      Entity_Factory  : aliased EntityFactoryQosPolicy;
      Resource_Limits : aliased SystemResourceLimitsQosPolicy;
   end record;
   pragma Convention (C, DomainParticipantFactoryQos);

   type DomainParticipantFactoryQos_Access is access constant DomainParticipantFactoryQos;

   DomainParticipantFactoryQos_INITIALIZER : constant DomainParticipantFactoryQos :=
                                               (Entity_Factory  => ENTITY_FACTORY_QOS_POLICY_DEFAULT,
                                                Resource_Limits => SYSTEM_RESOURCE_LIMITS_QOS_POLICY_DEFAULT);


   --  FLOW_CONTROLLER_PROPERTY_DEFAULT : constant FlowControllerProperty_T;
   --  pragma Interface(C, FLOW_CONTROLLER_PROPERTY_DEFAULT, "FLOW_CONTROLLER_PROPERTY_DEFAULT");

   --
   --  DLRL stuff
   --


   type ReferenceScope is
     (SIMPLE_CONTENT_SCOPE,
      REFERENCED_CONTENTS_SCOPE);

   type ObjectScope is
     (SIMPLE_OBJECT_SCOPE,
      CONTAINED_OBJECTS_SCOPE,
      RELATED_OBJECTS_SCOPE);

   type DCPSState is
     (INITIAL,
      REGISTERED,
      ENABLED);

   type CacheUsage is
     (READ_ONLY,
      WRITE_ONLY,
      READ_WRITE);

   type ObjectSubState is new Interfaces.Unsigned_16;

   OBJECT_NEW : constant ObjectSubState      := 1;
   OBJECT_MODIFIED : constant ObjectSubState     := 2;
   OBJECT_READ : constant ObjectSubState     := 4;
   OBJECT_DELETED : constant ObjectSubState     := 8;
   OBJECT_CREATED : constant ObjectSubState     := 256;
   OBJECT_CHANGED : constant ObjectSubState     := 512;
   OBJECT_WRITTEN : constant ObjectSubState     := 1024;
   OBJECT_DESTROYED : constant ObjectSubState     := 2048;

   type DLRLOid is record
      Creator_Id : aliased Unsigned_Long;
      Local_Id   : aliased Unsigned_Long;
   end record;

   type TimeOutDuration is    new Interfaces.Unsigned_32;

   INFINITE_TIME_OUT : constant TimeOutDuration  := -1;

   subtype ClassName is DDS.String;
   subtype CacheName is  DDS.String;
   subtype RelationName is DDS.String;

   BadParameter : exception;

   NotFound : exception;

   --  type ReadOnlyMode_Members is
   ReadOnlyMode : exception;

   WriteOnlyMode : exception;

   AlreadyClonedInWriteMode : exception;

   ExpiredTimeOut : exception;


   type ObjectReference is record
      Oid        : DLRLOid;
      Home_Index : Unsigned_Long;
   end record;

   type RelationKind is
     (REF_RELATION,
      LIST_RELATION,
      INT_MAP_RELATION,
      STR_MAP_RELATION);


   type RelatedObjectDepth is   new Short_Integer;
   UNLIMITED_RELATED_OBJECTS : constant RelatedObjectDepth    := -1;

   type MembershipState is
     (UNDEFINED_MEMBERSHIP,
      ALREADY_MEMBER,
      NOT_MEMBER);

   --
   --  end DLRL stuff
   --

private

   Not_Implemented : exception;
   --  type Wide_String is access all Standard.Wide_Character;

   --  private routines to get address of c-comaptible Qos
   function GetInterface (P_Qos : TopicQos) return System.Address;
   function GetInterface (P_Qos : DataWriterQos) return System.Address;
   function GetInterface (P_Qos : PublisherQos) return System.Address;
   function GetInterface (P_Qos : DataReaderQos) return System.Address;
   function GetInterface (P_Qos : SubscriberQos) return System.Address;
   function GetInterface (P_Qos : DomainParticipantQos) return System.Address;


end DDS;
