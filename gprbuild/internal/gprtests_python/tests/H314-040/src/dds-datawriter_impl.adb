pragma Ada_05;

with DDS.TypeSupport;
with DDS.Topic_Impl;
with DDS.Listener;
with DDS.Listener.Low_Level;
with DDS.DataWriterListener.Low_Level;

with Unchecked_Conversion;

package body DDS.DataWriter_Impl is

   use type System.Address;

   type Ref_Access_Access is access all Ref_Access;
   function C_DataWriter_Get_User_Data_I (C_DataWriter : System.Address)
                                         return Ref_Access_Access;
   pragma Import (C, C_DataWriter_Get_User_Data_I,
                    "DDS_Entity_get_user_dataI");

   -------------
   -- Set_Qos --
   -------------
   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataWriterQos) is
      function Internal
        (Self : System.Address;
         Qos  : System.Address)
        return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DataWriter_set_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Qos;

   -------------
   -- Get_Qos --
   -------------
   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataWriterQos) is
      function Internal
        (Self : System.Address;
         Qos  : System.Address)
        return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DataWriter_get_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Qos;


   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DataWriterListener.Ref_Access;
      Mask       : in DDS.StatusMask) is
      function C_Set_Listener
        (C_Writer   : System.Address;
         C_Listener : System.Address;
         Mask       : in DDS.StatusMask)
        return DDS.ReturnCode_T;
      pragma Import (C, C_Set_Listener, "DDS_DataWriter_set_listener");

      C_Listener : DDS.DataWriterListener.Low_Level.C_DataWriterListener :=
        DDS.DataWriterListener.Low_Level.C_DataWriterListener_DEFAULT;

   begin
      C_Listener.Listener := DDS.Listener.Low_Level.Listener_To_C
        (Listener.Ref_Access (A_Listener));
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
     return DDS.DataWriterListener.Ref_Access is

      procedure C_Get_Listener (C_DataWriter : in System.Address;
                                C_Listener   : in System.Address);
      pragma Import (C, C_Get_Listener,
                       "DDS_DataWriter_get_listenerX");

      C_Listener : DDS.DataWriterListener.Low_Level.C_DataWriterListener;

   begin
      C_Get_Listener (Self.GetInterface, C_Listener'Address);
      return DDS.DataWriterListener.Ref_Access
        (DDS.Listener.Low_Level.C_To_Listener (C_Listener.Listener));
   end Get_Listener;

   ---------------
   -- Get_Topic --
   ---------------

   function Get_Topic
     (Self : not null access Ref)
      return DDS.Topic.Ref_Access
   is
   begin
      return Get_Topic (Self);
   end Get_Topic;

   -------------------
   -- Get_Publisher --
   -------------------

   function Get_Publisher
     (Self : not null access Ref)
      return access DDS.Publisher.Ref'Class
   is
   begin
      return Get_Publisher (Self);
   end Get_Publisher;

   ------------------------------
   -- Wait_For_Acknowledgments --
   ------------------------------

   function Wait_For_Acknowledgments
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
      return DDS.ReturnCode_T
   is
   begin
      return Wait_For_Acknowledgments (Self, Max_Wait);
   end Wait_For_Acknowledgments;

   --------------------------------
   -- Get_Liveliness_Lost_Status --
   --------------------------------

   function Get_Liveliness_Lost_Status
     (Self : not null access Ref)
      return DDS.LivelinessLostStatus
   is
   begin
      return Get_Liveliness_Lost_Status (Self);
   end Get_Liveliness_Lost_Status;

   ----------------------------------------
   -- Get_Offered_Deadline_Missed_Status --
   ----------------------------------------

   function Get_Offered_Deadline_Missed_Status
     (Self : not null access Ref)
      return DDS.OfferedDeadlineMissedStatus
   is
   begin
      return Get_Offered_Deadline_Missed_Status (Self);
   end Get_Offered_Deadline_Missed_Status;

   -----------------------------------------
   -- Get_Offered_Incompatible_Qos_Status --
   -----------------------------------------

   function Get_Offered_Incompatible_Qos_Status
     (Self : not null access Ref)
      return DDS.OfferedIncompatibleQosStatus
   is
   begin
      return Get_Offered_Incompatible_Qos_Status (Self);
   end Get_Offered_Incompatible_Qos_Status;

   ----------------------------------
   -- Get_Publication_Match_Status --
   ----------------------------------

   function Get_Publication_Match_Status
     (Self : not null access Ref)
      return DDS.PublicationMatchedStatus
   is
   begin
      return Get_Publication_Match_Status (Self);
   end Get_Publication_Match_Status;

   -----------------------
   -- Assert_Liveliness --
   -----------------------

   function Assert_Liveliness
     (Self : not null access Ref)
      return DDS.ReturnCode_T
   is
   begin
      return Assert_Liveliness (Self);
   end Assert_Liveliness;

   -------------------------------
   -- Get_Matched_Subscriptions --
   -------------------------------

--     procedure Get_Matched_Subscriptions
--       (Self                 : not null access Ref;
--        Subscription_Handles : in out DDS.InstanceHandleSeq;
--        Returns              : out DDS.ReturnCode_T)
--     is
--     begin
--        null;
--     end Get_Matched_Subscriptions;

   -----------------------------------
   -- Get_Matched_Subscription_Data --
   -----------------------------------

   procedure Get_Matched_Subscription_Data
     (Self                : not null access Ref;
      Subscription_Data   : in DDS.SubscriptionBuiltinTopicData_Access;
      Subscription_Handle : in DDS.InstanceHandle_T;
      Returns             : out DDS.ReturnCode_T)
   is
   begin
      null;
   end Get_Matched_Subscription_Data;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self          : not null access Ref;
      Instance_Data : in System.Address;
      Handle        : in DDS.InstanceHandle_T_Access)
   is
      function Internal (Self                 : System.Address;
                         Remote_Reader_Handle : access InstanceHandle_T;
                         Writer_Info          : access OriginalWriterInfo_T;
                         Data                 : System.Address;
                         Handle               : access InstanceHandle_T)
                        return DDS.ReturnCode_T;

      pragma Import (C, Internal, "DDS_DataWriter_write_untyped_generalI");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface,
                                       null,
                                       null,
                                       Instance_Data,
                                       Handle),
                             "unable to write message");
   end Write;

   function CreateI (C_Publisher  : System.Address;
                     A_Topic      : access DDS.Topic.Ref'Class;
                     Qos          : in DDS.DataWriterQos;
                     A_Listener   : in DDS.DataWriterListener.Ref_Access;
                     Mask         : in DDS.StatusMask)
                    return DDS.DataWriter.Ref_Access is

      function C_Create_Disabled
        (Publisher   : System.Address;
         Need_Enable : DDS.Boolean_Ptr;
         A_Topic     : System.Address;
         Qos         : System.Address;
         A_Listener  : System.Address;
         Mask        : DDS.StatusMask)
         return System.Address;
      pragma Import (C, C_Create_Disabled,
                       "DDS_Publisher_create_datawriter_disabledI");

      function C_Writer_Get_TypeSupport (C_Writer : System.Address)
         return System.Address;
      pragma Import (C, C_Writer_Get_TypeSupport, "DDS_DataWriter_get_typeI");

      function C_Get_User_Data (C_TypeSupport : System.Address)
                                return System.Address;
      pragma Import (C, C_Get_User_Data, "DDS_DataTypeUtility_get_user_dataI");

      function A_To_T is new Unchecked_Conversion
        (Source => System.Address,
         Target => DDS.TypeSupport.Ref_Access);

      Need_To_Enable : aliased DDS.Boolean := False;
      TopicI : constant DDS.Topic_Impl.Ref_Access :=
                 DDS.Topic_Impl.Ref_Access (A_Topic);

      C_Writer_Ptr          : System.Address;
      C_DataTypeUtility_Ptr : System.Address;
      TypeSupport_Access    : DDS.TypeSupport.Ref_Access;
      Typed_Writer          : DDS.DataWriter.Ref_Access;
      Typed_Writer_Impl     : Ref_Access;
      Typed_Writer_Impl_A   : Ref_Access_Access;

      Created         : Boolean := False;

      use type DDS.DataWriterListener.Ref_Access;

   begin

      --  Create Disabled C Data Reader
      C_Writer_Ptr := C_Create_Disabled
        (Publisher   => C_Publisher,
         Need_Enable => Need_To_Enable'Unchecked_Access,
         A_Topic     => TopicI.GetInterface,
         Qos         => Qos.GetInterface,
         A_Listener  => System.Null_Address, --  will re-set after Ada object created and before enable
         Mask        => Mask);
      if C_Writer_Ptr /= System.Null_Address then

         C_DataTypeUtility_Ptr := C_Writer_Get_TypeSupport (C_Writer_Ptr);
         TypeSupport_Access := A_To_T (C_Get_User_Data (C_DataTypeUtility_Ptr));

         Typed_Writer := TypeSupport_Access.Create_TypedDataWriterI;
         Typed_Writer_Impl := DDS.DataWriter_Impl.Ref_Access (Typed_Writer);

         Typed_Writer_Impl.Entity_Initialize_I (C_Writer_Ptr);
         Typed_Writer_Impl_A :=  C_DataWriter_Get_User_Data_I (C_Writer_Ptr);

         if Typed_Writer_Impl_A /= null then
            Typed_Writer_Impl_A.all := Typed_Writer_Impl;

            --  Set up Listener
            if A_Listener /= null then
               Typed_Writer_Impl.Set_Listener (A_Listener, Mask);
            end if;

            if Need_To_Enable  then
               Typed_Writer_Impl.Enable;
            end if;
            Created := True;
         end if;
      end if;

      if Created  then
         return Typed_Writer;
      else
         --   RTI.Obj_Impl.Free (RTI.Obj_Impl.Ref_Access (Ret));
         raise DDS_ERROR with "Unable to Create DataWriter";
      end if;

   end CreateI;

   function Get_FacadeI (C_DataWriter : System.Address)
                        return Ref_Access is
      P_Impl : Ref_Access := null;
      P_Impl_Access : Ref_Access_Access;
   begin

      if C_DataWriter /= System.Null_Address then
         P_Impl_Access :=
           C_DataWriter_Get_User_Data_I (C_DataWriter);
         if P_Impl_Access /= null then
            P_Impl := P_Impl_Access.all;
         end if;
      end if;

      return P_Impl;

   end Get_FacadeI;

end DDS.DataWriter_Impl;
