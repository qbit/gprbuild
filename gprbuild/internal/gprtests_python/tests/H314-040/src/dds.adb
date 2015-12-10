pragma Ada_05;

with Ada.Exceptions; use Ada.Exceptions;

package body DDS is

   use type Interfaces.Unsigned_32;
   use type Interfaces.Integer_32;

   --  ====================================================================
   --  Suport routiones for short

   procedure Initialize (Self  : in out Short) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Short) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Short; Src : in Short) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for long

   procedure Initialize (Self  : in out Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Long; Src : in Long) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for long_long

   procedure Initialize (Self  : in out Long_Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Long_Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Long_Long; Src : in Long_Long) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Unsigned_short

   procedure Initialize (Self  : in out Unsigned_Short) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Unsigned_Short) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Unsigned_Short; Src : in Unsigned_Short) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Unsigned_long

   procedure Initialize (Self  : in out Unsigned_Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Unsigned_Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Unsigned_Long; Src : in Unsigned_Long) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for long_long

   procedure Initialize (Self  : in out Unsigned_Long_Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Unsigned_Long_Long) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Unsigned_Long_Long; Src : in Unsigned_Long_Long) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for float

   procedure Initialize (Self  : in out Float) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Float) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Float; Src : in Float) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Double

   procedure Initialize (Self  : in out Double) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Double) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Double; Src : in Double) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Long_Double

   procedure Initialize (Self  : in out Long_Double) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Long_Double) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Long_Double; Src : in Long_Double) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Char

   procedure Initialize (Self  : in out Char) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Char) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Char; Src : in Char) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Wchar

   procedure Initialize (Self  : in out Wchar) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Wchar) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Wchar; Src : in Wchar) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Octet

   procedure Initialize (Self  : in out Octet) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Octet) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Octet; Src : in Octet) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for Boolean

   procedure Initialize (Self  : in out Boolean) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out Boolean) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out Boolean; Src : in Boolean) is
   begin
      Dst := Src;
   end Copy;

   --  ====================================================================
   --  Suport routiones for String

   procedure Initialize (Self  : in out DDS.String) is
      pragma Unreferenced (Self);
   begin
      null;
   end Initialize;

   procedure Finalize (Self  : in out DDS.String) is
   begin
      Interfaces.C.Strings.Free (Self);
   end Finalize;

   procedure Copy (Dst : in out DDS.String; Src : in DDS.String) is
   begin
      Interfaces.C.Strings.Update
        (Item   => Dst,
         Offset => 0,
         Chars  => Interfaces.C.char_array'(Interfaces.C.Strings.Value (Src)),
         Check  => True);
   end Copy;



   function "<" (L : Time_T; R : Time_T) return Boolean is
   begin
      if L.Sec = R.Sec then
         return L.Nanosec < R.Nanosec;
      else
         return L.Sec < R.Sec;
      end if;
   end "<";

   function ">" (L : Time_T; R : Time_T) return Boolean is
   begin
      if L.Sec = R.Sec then
         return L.Nanosec > R.Nanosec;
      else
         return L.Sec > R.Sec;
      end if;
   end ">";

   function "<=" (L : Time_T; R : Time_T) return Boolean is
   begin
      if L.Sec < R.Sec then
         return True;
      elsif L.Sec > R.Sec then
         return False;
      else
         return L.Nanosec <= R.Nanosec;
      end if;
   end "<=";

   function ">=" (L : Time_T; R : Time_T) return Boolean is
   begin
      if L.Sec > R.Sec then
         return True;
      elsif L.Sec < R.Sec then
         return False;
      else
         return L.Nanosec >= R.Nanosec;
      end if;
   end ">=";


   function "+" (L : Time_T; R : Time_T) return Time_T is
      Sum : Time_T;
   begin

      Sum.Sec := L.Sec + R.Sec;
      Sum.Nanosec := L.Nanosec + R.Nanosec;
      if Sum.Nanosec > 1_000_000_000 then
         Sum.Sec := Sum.Sec + 1;
         Sum.Nanosec := Sum.Nanosec - 1_000_000_000;
      end if;

      return Sum;

   end "+";

   function Time_Is_Zero (T : Time_T) return Boolean is
   begin
      return (T = Time_Zero);
   end Time_Is_Zero;

   function Time_Is_Invalid (T : Time_T) return Boolean is
   begin
      return (T.Sec < 0);
   end Time_Is_Invalid;

   function Duration_Is_Zero (D : Duration_T) return Boolean is
   begin
      return (D = DURATION_ZERO);
   end Duration_Is_Zero;

   function Duration_Is_Infinite (D : Duration_T) return Boolean is
   begin
      return (D = DURATION_INFINITE);
   end Duration_Is_Infinite;


   ---------------------------
   -- Ret_Code_To_Exception --
   ---------------------------

   procedure Ret_Code_To_Exception
     (Code    : DDS.ReturnCode_T;
      Message : Standard.String := "")
   is
   begin
      case Code is
         when  RETCODE_OK =>
            null;
         when RETCODE_ERROR =>
            Raise_Exception (ERROR'Identity, Message);
         when RETCODE_UNSUPPORTED =>
            Raise_Exception (UNSUPPORTED'Identity, Message);
         when RETCODE_BAD_PARAMETER =>
            Raise_Exception (BAD_PARAMETER'Identity, Message);
         when RETCODE_PRECONDITION_NOT_MET =>
            Raise_Exception (PRECONDITION_NOT_MET'Identity, Message);
         when RETCODE_OUT_OF_RESOURCES =>
            Raise_Exception (OUT_OF_RESOURCES'Identity, Message);
         when RETCODE_NOT_ENABLED =>
            Raise_Exception (NOT_ENABLED'Identity, Message);
         when RETCODE_IMMUTABLE_POLICY =>
            Raise_Exception (IMMUTABLE_POLICY'Identity, Message);
         when RETCODE_INCONSISTENT_POLICY =>
            Raise_Exception (INCONSISTENT_POLICY'Identity, Message);
         when RETCODE_ALREADY_DELETED =>
            Raise_Exception (ALREADY_DELETED'Identity, Message);
         when RETCODE_TIMEOUT =>
            Raise_Exception (TIMEOUT'Identity, Message);
         when RETCODE_NO_DATA =>
            Raise_Exception (NO_DATA'Identity, Message);
            --  when others =>
            --   raise Program_Error with "Unknown DDS error code:" & Code'Img;
      end case;
   end Ret_Code_To_Exception;


   --  ===================================================================
   --  Suport routiones for InstanceHandle_T

   procedure Initialize (Self  : in out InstanceHandle_T) is
   begin
      Self := DDS_Support.Null_InstanceHandle_T;
   end Initialize;
   procedure Finalize (Self  : in out InstanceHandle_T) is
   begin
      Self := DDS_Support.Null_InstanceHandle_T;
   end Finalize;
   procedure Copy (Dst : in out InstanceHandle_T; Src : in InstanceHandle_T) is
   begin
      Dst := Src;
   end Copy;

   --  ===================================================================
   --  Suport routiones for SequenceNumber_T

   --  #define DDS_SequenceNumber_compare(sn1,sn2) \
   --    ((((sn1)->high) > ((sn2)->high)) ? 1 : \
   --     ((((sn1)->high) < ((sn2)->high)) ? -1 : \
   --      ((((sn1)->low) > ((sn2)->low)) ? 1 : \
   --       ((((sn1)->low) < ((sn2)->low)) ? -1 : 0))))

   function SequenceNumber_Compare (Sn1 : SequenceNumber_T_Access;
                                    Sn2 : SequenceNumber_T_Access)
                                    return Long is
   begin
      if Sn1.High > Sn2.High then
         return 1;
      elsif Sn1.High < Sn2.High then
         return -1;
      elsif     Sn1.Low > Sn2.Low then
         return 1;
      elsif     Sn1.Low < Sn2.Low then
         return -1;
      else
         return 0;
      end if;
   end SequenceNumber_Compare;



   --  ====================================================================
   --  Suport routiones for QosPolicyCount

   procedure Initialize (Self  : in out QosPolicyCount) is
      pragma Unreferenced (Self);
   begin
      Self := (INVALID_QOS_POLICY_ID, 0);
   end Initialize;

   procedure Finalize (Self  : in out QosPolicyCount) is
   begin
      Self := (INVALID_QOS_POLICY_ID, 0);
   end Finalize;

   procedure Copy (Dst : in out QosPolicyCount; Src : in QosPolicyCount) is
   begin
      Dst := Src;
   end Copy;



   --  ====================================================================
   --  Suport routiones for SampleInfo

   procedure Initialize (Self  : in out SampleInfo) is
      pragma Unreferenced (Self);
   begin
      Self := (Sample_State                        => 0,
               View_State                          => 0,
               Instance_State                      => 0,
               Source_Timestamp                    => Time_Zero,
               Instance_Handle                     => Null_InstanceHandle_T,
               Publication_Handle                  => Null_InstanceHandle_T,
               Disposed_Generation_Count           => 0,
               No_Writers_Generation_Count         => 0,
               Sample_Rank                         => 0,
               Generation_Rank                     => 0,
               Absolute_Generation_Rank            => 0,
               Valid_Data                          => False,
               Spare1                              => False,
               Spare2                              => False,
               Spare3                              => False,
               Reception_Timestamp                 => Time_Zero,
               Publication_Sequence_Number         => SEQUENCE_NUMBER_UNKNOWN,
               Reception_Sequence_Number           => SEQUENCE_NUMBER_UNKNOWN,
               Publication_Virtual_Guid            => (Value => (others => 0)),
               Publication_Virtual_Sequence_Number => SEQUENCE_NUMBER_UNKNOWN);
   end Initialize;

   procedure Finalize (Self  : in out SampleInfo) is
      pragma Unreferenced (Self);
   begin
      null;
   end Finalize;

   procedure Copy (Dst : in out SampleInfo; Src : in SampleInfo) is
   begin
      Dst := Src;
   end Copy;


   --  ====================================================================
   --  Suport routiones for TransportUnicastSettings_T

   procedure Initialize (Self  : in out TransportUnicastSettings_T) is
   begin
      String_Seq.Initialize (Self.Transports'Unrestricted_Access);
      Self.Receive_Port := 0;
   end Initialize;

   procedure Finalize (Self  : in out TransportUnicastSettings_T) is
   begin
      String_Seq.Finalize (Self.Transports'Unrestricted_Access);
      Self.Receive_Port := 0;
   end Finalize;

   procedure Copy (Dst : in out TransportUnicastSettings_T; Src : in TransportUnicastSettings_T) is
   begin
      String_Seq.Copy (Dst.Transports'Unrestricted_Access,
                       Src.Transports'Unrestricted_Access);
      Dst.Receive_Port := Src.Receive_Port;
   end Copy;

   --  ====================================================================
   --  Suport routiones for TransportMulticastSettings_T

   procedure Initialize (Self  : in out TransportMulticastSettings_T) is
   begin
      Self := (String_Seq.DEFAULT_SEQUENCE,
               Interfaces.C.Strings.Null_Ptr,
               0);
   end Initialize;

   procedure Finalize (Self  : in out TransportMulticastSettings_T) is
   begin
      String_Seq.Finalize (Self.Transports'Unrestricted_Access);
      Interfaces.C.Strings.Free (Self.Receive_Address);
      Self.Receive_Port := 0;
   end Finalize;

   procedure Copy (Dst : in out TransportMulticastSettings_T; Src : in TransportMulticastSettings_T) is
      use Interfaces.C.Strings;
   begin
      String_Seq.Copy (Dst.Transports'Unrestricted_Access,
                       Src.Transports'Unrestricted_Access);
      Dst.Receive_Address := New_String (Value (Src.Receive_Address));
      Dst.Receive_Port := Src.Receive_Port;

   end Copy;

   --  ====================================================================
   --  Suport routiones for Discovery_ParticipantInformation

   procedure Initialize (Self : in out Discovery_ParticipantInformation) is
   begin
      Self.Participant_Discovery_Id := 0;
      Self.Participant_Discovery_Version := 0;
      Self.Participant_Discovery_Vendor_Id := 0;
      Octet_Seq.Initialize (Self.Participant_Discovery_Parameters'Unrestricted_Access);
   end Initialize;

   procedure Finalize (Self : in out Discovery_ParticipantInformation) is
   begin
      Self.Participant_Discovery_Id := 0;
      Self.Participant_Discovery_Version := 0;
      Self.Participant_Discovery_Vendor_Id := 0;
      Octet_Seq.Finalize (Self.Participant_Discovery_Parameters'Unrestricted_Access);
   end  Finalize;

   procedure Copy (Dst : in out Discovery_ParticipantInformation;
                   Src : in Discovery_ParticipantInformation) is
   begin
      Dst.Participant_Discovery_Id := Src.Participant_Discovery_Id;
      Dst.Participant_Discovery_Version := Src.Participant_Discovery_Version;
      Dst.Participant_Discovery_Vendor_Id := Src.Participant_Discovery_Vendor_Id;
      Octet_Seq.Copy (Dst.Participant_Discovery_Parameters'Unrestricted_Access,
                      Src.Participant_Discovery_Parameters'Unrestricted_Access);
   end Copy;

   --  ====================================================================
   --  Suport routiones for Discovery_EndpointInformation

   procedure Initialize (Self : in out Discovery_EndpointInformation) is
   begin
      Self.Endpoint_Discovery_Id := 0;
      Self.Endpoint_Discovery_Version := 0;
      Self.Endpoint_Discovery_Vendor_Id := 0;
      Octet_Seq.Initialize (Self.Endpoint_Discovery_Parameters'Unrestricted_Access);
   end Initialize;

   procedure Finalize (Self : in out Discovery_EndpointInformation) is
   begin
      Self.Endpoint_Discovery_Id := 0;
      Self.Endpoint_Discovery_Version := 0;
      Self.Endpoint_Discovery_Vendor_Id := 0;
      Octet_Seq.Finalize (Self.Endpoint_Discovery_Parameters'Unrestricted_Access);
   end Finalize;

   procedure Copy (Dst : in out Discovery_EndpointInformation;
                   Src : in Discovery_EndpointInformation) is
   begin
      Dst.Endpoint_Discovery_Id := Src.Endpoint_Discovery_Id;
      Dst.Endpoint_Discovery_Version := Src.Endpoint_Discovery_Version;
      Dst.Endpoint_Discovery_Vendor_Id := Src.Endpoint_Discovery_Vendor_Id;
      Octet_Seq.Copy (Dst.Endpoint_Discovery_Parameters'Unrestricted_Access,
                      Src.Endpoint_Discovery_Parameters'Unrestricted_Access);
   end Copy;

   --  ====================================================================
   --  Suport routiones for Locator_T
   procedure Initialize (Self  : in out Locator_T) is
   begin
      Self := LOCATOR_INVALID;
   end Initialize;
   procedure Finalize (Self  : in out Locator_T) is
   pragma Unreferenced (Self);
   begin
      null;
   end Finalize;
   procedure Copy (Dst : in out Locator_T; Src : in Locator_T) is
   begin
      Dst := Src;
   end Copy;


   --  ====================================================================
   --  Suport routiones for Property_T

   procedure Initialize (Self  : in out Property_T) is
   begin
      Interfaces.C.Strings.Free (Self.Name);
      Interfaces.C.Strings.Free (Self.Value);
   end Initialize;

   procedure Finalize (Self  : in out Property_T) is
   begin
      Interfaces.C.Strings.Free (Self.Name);
      Interfaces.C.Strings.Free (Self.Value);
   end Finalize;

   procedure Copy (Dst : in out Property_T; Src : in Property_T) is
      use Interfaces.C.Strings;
   begin
      Dst.Name := New_String (Value (Src.Name));
      Dst.Value := New_String (Value (Src.Value));
   end Copy;

   function BuiltinTopicKey_Equals (A : in BuiltinTopicKey_T_Access;
                                    B : in BuiltinTopicKey_T_Access)
                                    return DDS.Boolean is
   begin
      if ((A.Value (0) = B.Value (0))  and
            (A.Value (1) = B.Value (1))  and
            (A.Value (2) = B.Value (2))  and
            (A.Value (3) = B.Value (3)))  then
         return TRUE;
      else
         return FALSE;
      end if;
   end BuiltinTopicKey_Equals;


   --  Qos Subprograms

   procedure Initialize (Self : in out TopicQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_TopicQos_initialize");
   begin
      Internal (Self.GetInterface);
   end Initialize;

   procedure Finalize (Self : in out TopicQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_TopicQos_finalize");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Copy (Dst : in out TopicQos;
                   Src : in out TopicQos) is
      procedure Internal (Dst_Qos_Ptr : System.Address;
                          Src_Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_TopicQos_copy");
   begin
      Internal (Dst.GetInterface, Src.GetInterface);
   end Copy;

   procedure Initialize (Self : in out DataWriterQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DataWriterQos_initialize");
   begin
      Internal (Self.GetInterface);
   end Initialize;

   procedure Finalize (Self : in out DataWriterQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DataWriterQos_finalize");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Copy (Dst : in out DataWriterQos;
                   Src : in out DataWriterQos) is
      procedure Internal (Dst_Qos_Ptr : System.Address;
                          Src_Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DataWriterQos_copy");
   begin
      Internal (Dst.GetInterface, Src.GetInterface);
   end Copy;

   procedure Initialize (Self : in out PublisherQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_PublisherQos_initialize");
   begin
      Internal (Self.GetInterface);
   end Initialize;

   procedure Finalize (Self : in out PublisherQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_PublisherQos_finalize");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Copy (Dst : in out PublisherQos;
                   Src : in out PublisherQos) is
      procedure Internal (Dst_Qos_Ptr : System.Address;
                          Src_Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_PublisherQos_copy");
   begin
      Internal (Dst.GetInterface, Src.GetInterface);
   end Copy;

   procedure Initialize (Self : in out DataReaderQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DataReaderQos_initialize");
   begin
      Internal (Self.GetInterface);
   end Initialize;

   procedure Finalize (Self : in out DataReaderQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DataReaderQos_finalize");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Copy (Dst : in out DataReaderQos;
                   Src : in out DataReaderQos) is
      procedure Internal (Dst_Qos_Ptr : System.Address;
                          Src_Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DataReaderQos_copy");
   begin
      Internal (Dst.GetInterface, Src.GetInterface);
   end Copy;

   procedure Initialize (Self : in out SubscriberQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_SubscriberQos_initialize");
   begin
      Internal (Self.GetInterface);
   end Initialize;

   procedure Finalize (Self : in out SubscriberQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_SubscriberQos_finalize");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Copy (Dst : in out SubscriberQos;
                   Src : in out SubscriberQos) is
      procedure Internal (Dst_Qos_Ptr : System.Address;
                          Src_Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_SubscriberQos_copy");
   begin
      Internal (Dst.GetInterface, Src.GetInterface);
   end Copy;

   procedure Initialize (Self : in out DomainParticipantQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DomainParticipantQos_initialize");
   begin
      Internal (Self.GetInterface);
   end Initialize;

   procedure Finalize (Self : in out DomainParticipantQos) is
      procedure Internal (Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DomainParticipantQos_finalize");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Copy (Dst : in out DomainParticipantQos;
                   Src : in DomainParticipantQos) is
      procedure Internal (Dst_Qos_Ptr : System.Address;
                          Src_Qos_Ptr : System.Address);
      pragma Import (C, Internal, "DDS_DomainParticipantQos_copy");
   begin
      Internal (Dst.GetInterface, Src.GetInterface);
   end Copy;

   --  Participant Qos Private Subprograms
   function GetInterface (P_Qos : TopicQos) return System.Address is   begin
      return P_Qos.Topic_Data'Address;
   end GetInterface;

   function GetInterface (P_Qos : DataWriterQos) return System.Address is
   begin
      return P_Qos.Durability'Address;
   end GetInterface;

   function GetInterface (P_Qos : PublisherQos) return System.Address is
   begin
      return P_Qos.Presentation'Address;
   end GetInterface;

   function GetInterface (P_Qos : DataReaderQos) return System.Address is
   begin
      return P_Qos.Durability'Address;
   end GetInterface;

   function GetInterface (P_Qos : SubscriberQos) return System.Address is
   begin
      return P_Qos.Presentation'Address;
   end GetInterface;

   function GetInterface (P_Qos : in DomainParticipantQos) return System.Address is
   begin
      return P_Qos.User_Data'Address;
   end GetInterface;


begin
   null;
end DDS;
