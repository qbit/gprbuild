pragma Ada_05;

with DDS.TypeSupport;
with DDS.Topic_Impl;
with DDS.Listener;
with DDS.Listener.Low_Level;
with DDS.DataReaderListener.Low_Level;

with Unchecked_Conversion;

package body DDS.DataReader_Impl is

   use type System.Address;

   type Ref_Access_Access is access all Ref_Access;
   function C_DataReader_Get_User_Data_I (C_DataReader : System.Address)
                                         return Ref_Access_Access;
   pragma Import (C, C_DataReader_Get_User_Data_I,
                    "DDS_Entity_get_user_dataI");

   procedure SetSubscriber (Self       : not null access Ref;
                            Subscriber : access DDS.Subscriber.Ref'Class) is
   begin
      null; -- Self.Subscriber := Subscriber.all'Unchecked_Access;
   end SetSubscriber;

   --------------------------
   -- Create_Readcondition --
   --------------------------

   function Create_Readcondition
     (Self            : not null access Ref;
      Sample_States   : in DDS.SampleStateMask;
      View_States     : in DDS.ViewStateMask;
      Instance_States : in DDS.InstanceStateMask)
      return DDS.ReadCondition.Ref_Access is
   begin
      raise Program_Error with "not  implemented";
      return Create_Readcondition (Self, Sample_States, View_States,
                                   Instance_States);
   end Create_Readcondition;

   ---------------------------
   -- Create_Querycondition --
   ---------------------------

   function Create_Querycondition
     (Self             : not null access Ref;
      Sample_States    : in DDS.SampleStateMask;
      View_States      : in DDS.ViewStateMask;
      Instance_States  : in DDS.InstanceStateMask;
      Query_Expression : in DDS.String;
      Query_Parameters : in DDS.String_Seq.Sequence)
      return DDS.QueryCondition.Ref_Access is
   begin
      raise Program_Error with "not  implemented";
      return Create_Querycondition (Self, Sample_States, View_States,
                                    Instance_States, Query_Expression, Query_Parameters);
   end Create_Querycondition;

   --------------------------
   -- Delete_Readcondition --
   --------------------------

   function Delete_Readcondition
     (Self        : not null access Ref;
      A_Condition : in DDS.ReadCondition.Ref_Access)
      return DDS.ReturnCode_T is
   begin
      raise Program_Error with "not  implemented";
      return Delete_Readcondition (Self, A_Condition);
   end Delete_Readcondition;

   -------------------------------
   -- Delete_Contained_Entities --
   -------------------------------

   function Delete_Contained_Entities
     (Self : not null access Ref)
     return DDS.ReturnCode_T is

      function Internal (Self : System.Address) return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DataReader_delete_contained_entities");

   begin
      return Internal (Self.GetInterface);
   end Delete_Contained_Entities;

   procedure  Delete_Contained_Entities
     (Self : not null access Ref) is
   begin
      Ret_Code_To_Exception (Self.Delete_Contained_Entities);
   end Delete_Contained_Entities;


   -------------
   -- Set_Qos --
   -------------

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataReaderQos) is
      function Internal (Self  : System.Address;
                         Qos   : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_DataReader_set_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Qos;

   -------------
   -- Get_Qos --
   -------------

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataReaderQos) is
      function Internal (Self  : System.Address;
                         Qos   : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_DataReader_get_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Qos;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DataReaderListener.Ref_Access;
      Mask       : in DDS.StatusMask) is

      function C_Set_Listener (C_Reader   : in System.Address;
                               C_Listener : in System.Address;
                               Mask       : in DDS.StatusMask)
        return DDS.ReturnCode_T;
      pragma Import (C, C_Set_Listener, "DDS_DataReader_set_listener");

      C_Listener : DDS.DataReaderListener.Low_Level.C_DataReaderListener :=
        DDS.DataReaderListener.Low_Level.C_DataReaderListener_DEFAULT;

   begin
      C_Listener.Listener := DDS.Listener.Low_Level.Listener_To_C
        (DDS.Listener.Ref_Access (A_Listener));
      if Self.GetInterface /= System.Null_Address then
            Ret_Code_To_Exception (C_Set_Listener (Self.GetInterface,
                                                   C_Listener'Address,
                                                   Mask),
                                   "Set Listener FAILED");
      end if;
   end Set_Listener;

   ------------------
   -- Get_Listener --
   ------------------

   function Get_Listener
     (Self : not null access Ref)
     return DDS.DataReaderListener.Ref_Access is

      procedure C_Get_Listener (C_DataReader : in System.Address;
                                C_Listener   : in System.Address);
      pragma Import (C, C_Get_Listener,
                       "DDS_DataReader_get_listenerX");

      C_Listener : DDS.DataReaderListener.Low_Level.C_DataReaderListener;

   begin
      C_Get_Listener (Self.GetInterface, C_Listener'Address);
      return DDS.DataReaderListener.Ref_Access
        (DDS.Listener.Low_Level.C_To_Listener (C_Listener.Listener));
   end Get_Listener;

   --------------------------
   -- Get_Topicdescription --
   --------------------------

   function Get_Topicdescription
     (Self : not null access Ref)
      return DDS.TopicDescription.Ref_Access
   is
   begin
      raise Program_Error with "not  implemented";
      return Get_Topicdescription (Self);
   end Get_Topicdescription;



   --------------------------------
   -- Get_Sample_Rejected_Status --
   --------------------------------

   function Get_Sample_Rejected_Status
     (Self : not null access Ref)
      return DDS.SampleRejectedStatus
   is
   begin
      raise Program_Error with "not  implemented";
      return Get_Sample_Rejected_Status (Self);
   end Get_Sample_Rejected_Status;

   -----------------------------------
   -- Get_Liveliness_Changed_Status --
   -----------------------------------

   function Get_Liveliness_Changed_Status
     (Self : not null access Ref)
      return DDS.LivelinessChangedStatus
   is
   begin
      raise Program_Error with "not  implemented";
      return Get_Liveliness_Changed_Status (Self);
   end Get_Liveliness_Changed_Status;

   ------------------------------------------
   -- Get_Requested_Deadline_Missed_Status --
   ------------------------------------------

   function Get_Requested_Deadline_Missed_Status
     (Self : not null access Ref)
      return DDS.RequestedDeadlineMissedStatus
   is
   begin
      raise Program_Error with "not  implemented";
      return Get_Requested_Deadline_Missed_Status (Self);
   end Get_Requested_Deadline_Missed_Status;

   -------------------------------------------
   -- Get_Requested_Incompatible_Qos_Status --
   -------------------------------------------

   function Get_Requested_Incompatible_Qos_Status
     (Self : not null access Ref)
      return DDS.RequestedIncompatibleQosStatus
   is
   begin
      raise Program_Error with "not  implemented";
      return Get_Requested_Incompatible_Qos_Status (Self);
   end Get_Requested_Incompatible_Qos_Status;

   -----------------------------------
   -- Get_Subscription_Match_Status --
   -----------------------------------

   function Get_Subscription_Match_Status
     (Self : not null access Ref)
      return DDS.SubscriptionMatchedStatus
   is
   begin
      raise Program_Error with "not  implemented";
      return Get_Subscription_Match_Status (Self);
   end Get_Subscription_Match_Status;

   ----------------------------
   -- Get_Sample_Lost_Status --
   ----------------------------

   function Get_Sample_Lost_Status
     (Self : not null access Ref)
      return DDS.SampleLostStatus
   is
   begin
      raise Program_Error with "not  implemented";
      return Get_Sample_Lost_Status (Self);
   end Get_Sample_Lost_Status;

   --------------------
   -- Get_Subscriber --
   --------------------

   function Get_Subscriber
     (Self : not null access Ref)
      return access DDS.Subscriber.Ref'Class is
   begin
      raise Program_Error with "not  implemented";
      return Get_Subscriber (Self);
   end Get_Subscriber;

   ------------------------------
   -- Wait_For_Historical_Data --
   ------------------------------

   function Wait_For_Historical_Data
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
      return DDS.ReturnCode_T
   is
   begin
      raise Program_Error with "not  implemented";
      return Wait_For_Historical_Data (Self, Max_Wait);
   end Wait_For_Historical_Data;

   ------------------------------
   -- Get_Matched_Publications --
   ------------------------------

   procedure Get_Matched_Publications
     (Self                : not null access Ref;
      Publication_Handles : in out DDS.InstanceHandle_Seq.Sequence;
      Returns             : out DDS.ReturnCode_T)
   is
   begin
      raise Program_Error with "not  implemented";
   end Get_Matched_Publications;

   ----------------------------------
   -- Get_Matched_Publication_Data --
   ----------------------------------

   procedure Get_Matched_Publication_Data
     (Self               : not null access Ref;
      Publication_Data   : in out DDS.PublicationBuiltinTopicData;
      Publication_Handle : in DDS.InstanceHandle_T;
      Returns            : out DDS.ReturnCode_T) is
   begin
      raise Program_Error with "not  implemented";
   end Get_Matched_Publication_Data;

   function CreateI (C_Subscriber : System.Address;
                     A_Topic      : not null DDS.Topic.Ref_Access;
                     Qos          : in DDS.DataReaderQos;
                     A_Listener   : in DDS.DataReaderListener.Ref_Access;
                     Mask         : in DDS.StatusMask)
                    return DDS.DataReader.Ref_Access is

      function C_Create_Disabled
        (Subscriber  : System.Address;
         Need_Enable : DDS.Boolean_Ptr;
         A_Topic     : System.Address;
         Qos         : System.Address;
         A_Listener  : System.Address;
         Mask        : DDS.StatusMask)
         return System.Address;
      pragma Import (C, C_Create_Disabled,
                       "DDS_Subscriber_create_datareader_disabledI");

      function C_Reader_Get_TypeSupport (C_Reader : System.Address)
         return System.Address;
      pragma Import (C, C_Reader_Get_TypeSupport, "DDS_DataReader_get_typeI");

      function C_Get_User_Data (C_TypeSupport : System.Address)
                                return System.Address;
      pragma Import (C, C_Get_User_Data, "DDS_DataTypeUtility_get_user_dataI");

      function A_To_T is new Unchecked_Conversion
        (Source => System.Address,
         Target => DDS.TypeSupport.Ref_Access);

      Need_To_Enable : aliased DDS.Boolean := False;

      Topic : constant DDS.Topic_Impl.Ref_Access :=
        DDS.Topic_Impl.Ref_Access (A_Topic);

      C_Reader_Ptr          : System.Address;
      C_DataTypeUtility_Ptr : System.Address;
      TypeSupport_Access    : DDS.TypeSupport.Ref_Access;
      Typed_Reader          : DDS.DataReader.Ref_Access;
      Typed_Reader_Impl     : Ref_Access;
      Typed_Reader_Impl_A   : Ref_Access_Access;

      Created         : Boolean := False;

      use type DDS.DataReaderListener.Ref_Access;

   begin

      --  Create Disabled C Data Reader
      C_Reader_Ptr := C_Create_Disabled
        (Subscriber  => C_Subscriber,
         Need_Enable => Need_To_Enable'Unchecked_Access,
         A_Topic     => Topic_Impl.As_C_TopicDescriptionI (Topic.GetInterface),
         Qos         => Qos.GetInterface,
         A_Listener  => System.Null_Address, --  will re-set after Ada object created and before enable
         Mask        => Mask);

      if C_Reader_Ptr /= System.Null_Address then

         C_DataTypeUtility_Ptr := C_Reader_Get_TypeSupport (C_Reader_Ptr);
         TypeSupport_Access := A_To_T (C_Get_User_Data (C_DataTypeUtility_Ptr));

         Typed_Reader := TypeSupport_Access.Create_TypedDataReaderI;
         Typed_Reader_Impl := DDS.DataReader_Impl.Ref_Access (Typed_Reader);

         Typed_Reader_Impl.Entity_Initialize_I (C_Reader_Ptr);
         Typed_Reader_Impl_A :=  C_DataReader_Get_User_Data_I (C_Reader_Ptr);
         if Typed_Reader_Impl_A /= null then
            Typed_Reader_Impl_A.all := Typed_Reader_Impl;

            --  Set up Listener
            if A_Listener /= null then
               Typed_Reader_Impl.Set_Listener (A_Listener, Mask);
            end if;

            if Need_To_Enable  then
               Typed_Reader_Impl.Enable;
            end if;
            Created := True;
         end if;
      end if;

      if Created  then
         return Typed_Reader;
      else
         --   RTI.Obj_Impl.Free (RTI.Obj_Impl.Ref_Access (Ret));
         raise DDS_ERROR with "Unable to Create Datareader";
      end if;

   end CreateI;

   function Get_FacadeI (C_DataReader : System.Address)
                        return Ref_Access is
      P_Impl : Ref_Access := null;
      P_Impl_Access : Ref_Access_Access;
   begin

      if C_DataReader /= System.Null_Address then
         P_Impl_Access :=
           C_DataReader_Get_User_Data_I (C_DataReader);
         if P_Impl_Access /= null then
            P_Impl := P_Impl_Access.all;
         end if;
      end if;

      return P_Impl;

   end Get_FacadeI;

end DDS.DataReader_Impl;
