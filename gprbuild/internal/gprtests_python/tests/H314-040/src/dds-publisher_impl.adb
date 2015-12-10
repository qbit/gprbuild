pragma Ada_05;

with DDS.DataWriter_Impl;
with DDS.Listener;
with DDS.Listener.Low_Level;
with DDS.PublisherListener.Low_Level;

with System;

package body DDS.Publisher_Impl is

   use type System.Address;

   type Ref_Access_Access is access all Ref_Access;
   function C_Publisher_Get_User_Data_I (C_Publisher : System.Address)
                                        return Ref_Access_Access;
   pragma Import (C, C_Publisher_Get_User_Data_I,
                    "DDS_Entity_get_user_dataI");

   -----------------------
   -- Create_DataWriter --
   -----------------------

   function Create_DataWriter
     (Self       : not null  access Ref;
      A_Topic    : in DDS.Topic.Ref_Access;
      Qos        : in DDS.DataWriterQos;
      A_Listener : in DDS.DataWriterListener.Ref_Access;
      Mask       : in DDS.StatusMask)
     return DDS.DataWriter.Ref_Access is
   begin

      return DDS.DataWriter_Impl.CreateI (Self.GetInterface,
                                          A_Topic,
                                          Qos,
                                          A_Listener,
                                          Mask);
   end Create_DataWriter;

   -----------------------
   -- Delete_DataWriter --
   -----------------------

   procedure Delete_DataWriter
     (Self         : not null  access Ref;
      A_DataWriter : in out DDS.DataWriter.Ref_Access)
   is
      function Internal
        (Self       : System.Address;
         DataWriter : System.Address)
        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Publisher_delete_datawriter");
      use DataWriter;

      W : DDS.DataWriter_Impl.Ref_Access :=
        DDS.DataWriter_Impl.Ref_Access (A_DataWriter);

   begin
      if A_DataWriter /= null then
         Ret_Code_To_Exception (Internal (Self.GetInterface, W.GetInterface));
         DataWriter_Impl.Free (W);
         A_DataWriter := DDS.DataWriter.Ref_Access (W);
      end if;
   end Delete_DataWriter;

   -----------------------
   -- Lookup_DataWriter --
   -----------------------

   function Lookup_DataWriter
     (Self       : not null  access Ref;
      Topic_Name : in DDS.String_Ptr)
     return DDS.DataWriter.Ref_Access
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return null;
   end Lookup_DataWriter;

   -------------------------------
   -- Delete_Contained_Entities --
   -------------------------------

   function Delete_Contained_Entities
     (Self : not null  access Ref)
     return DDS.ReturnCode_T
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.RETCODE_UNSUPPORTED;
   end Delete_Contained_Entities;

   -------------
   -- Set_Qos --
   -------------

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos)
   is
      function Internal
        (Self  : System.Address;
         Qos   : System.Address)
        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Publisher_set_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Qos;

   -------------
   -- Get_Qos --
   -------------

   procedure Get_Qos
     (Self : not null  access Ref;
      Qos  : in DDS.PublisherQos)
   is
      function Internal
        (Self  : System.Address;
         Qos   : System.Address)
        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Publisher_get_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Qos;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self       : not null  access Ref;
      A_Listener : in DDS.PublisherListener.Ref_Access;
      Mask       : in DDS.StatusMask)
   is
      function C_Set_Listener (C_Publisher : in System.Address;
                               C_Listener   : in System.Address;
                               Mask         : in DDS.StatusMask)
                              return DDS.ReturnCode_T;
      pragma Import (C, C_Set_Listener, "DDS_Publisher_set_listener");

      C_Listener : DDS.PublisherListener.Low_Level.C_PublisherListener :=
        DDS.PublisherListener.Low_Level.C_PublisherListener_DEFAULT;

   begin
      C_Listener.As_DataWriterListener.Listener := DDS.Listener.Low_Level.Listener_To_C
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
     (Self : not null  access Ref)
     return DDS.PublisherListener.Ref_Access is

      C_Listener : DDS.PublisherListener.Low_Level.C_PublisherListener;

      procedure C_Get_Listener (C_Publisher : in System.Address;
                                C_Listener   : in System.Address);
      pragma Import (C, C_Get_Listener,
                       "DDS_Publisher_get_listenerX");
   begin
      C_Get_Listener (Self.GetInterface, C_Listener'Address);
      return DDS.PublisherListener.Ref_Access
        (DDS.Listener.Low_Level.C_To_Listener (C_Listener.As_DataWriterListener.Listener));
   end Get_Listener;

   --------------------------
   -- Suspend_Publications --
   --------------------------

   function Suspend_Publications
     (Self : not null  access Ref)
     return DDS.ReturnCode_T
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.RETCODE_UNSUPPORTED;
   end Suspend_Publications;

   -------------------------
   -- Resume_Publications --
   -------------------------

   function Resume_Publications
     (Self : not null  access Ref)
     return DDS.ReturnCode_T
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.RETCODE_UNSUPPORTED;
   end Resume_Publications;

   ----------------------------
   -- Begin_Coherent_Changes --
   ----------------------------

   function Begin_Coherent_Changes
     (Self : not null  access Ref)
     return DDS.ReturnCode_T
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.RETCODE_UNSUPPORTED;
   end Begin_Coherent_Changes;

   --------------------------
   -- End_Coherent_Changes --
   --------------------------

   function End_Coherent_Changes
     (Self : not null  access Ref)
     return DDS.ReturnCode_T
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.RETCODE_UNSUPPORTED;
   end End_Coherent_Changes;

   ------------------------------
   -- Wait_For_Acknowledgments --
   ------------------------------

   function Wait_For_Acknowledgments
     (Self     : not null  access Ref;
      Max_Wait : in DDS.Duration_T)
     return DDS.ReturnCode_T
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.RETCODE_UNSUPPORTED;
   end Wait_For_Acknowledgments;

   ---------------------
   -- Get_Participant --
   ---------------------

   function Get_Participant
     (Self : not null  access Ref)
     return access DDS.DomainParticipant.Ref'Class
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return null;
   end Get_Participant;

   --------------------------------
   -- Set_Default_DataWriter_Qos --
   --------------------------------

   procedure Set_Default_DataWriter_Qos
     (Self : not null  access Ref;
      Qos  : in DDS.DataWriterQos)
   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Publisher_set_default_datawriter_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Default_DataWriter_Qos;

   --------------------------------
   -- Get_Default_DataWriter_Qos --
   --------------------------------

   procedure Get_Default_DataWriter_Qos
     (Self : not null  access Ref;
      Qos  : in out DDS.DataWriterQos)
   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Publisher_get_default_datawriter_qos");

   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Default_DataWriter_Qos;

   -------------------------
   -- Copy_From_Topic_Qos --
   -------------------------

   procedure Copy_From_Topic_Qos
     (Self               : not null access Ref;
      A_DataWriter_Qos   : in out DDS.DataWriterQos;
      A_Topic_Qos        : in DDS.TopicQos) is
      function Internal (Self   : System.Address;
                         D_Qos  : System.Address;
                         T_Qos  : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Publisher_copy_from_topic_qos");
   begin
      Ret_Code_To_Exception
        (Internal (Self.GetInterface,
                   A_DataWriter_Qos.GetInterface,
                   A_Topic_Qos.GetInterface));
   end Copy_From_Topic_Qos;

   function CreateI (C_Participant : in System.Address;
                     Qos           : in DDS.PublisherQos;
                     A_Listener    : in DDS.PublisherListener.Ref_Access;
                     Mask          : in DDS.StatusMask)
                    return DDS.Publisher.Ref_Access is

      function C_Create_Disabled (Participant : System.Address;
                                  Need_Enable : DDS.Boolean_Ptr;
                                  Qos         : System.Address;
                                  C_Listener  : System.Address;
                                  Mask        : DDS.StatusMask)
                                 return System.Address;
      pragma Import (C, C_Create_Disabled,
                       "DDS_DomainParticipant_create_publisher_disabledI");

      C_Publisher_Ptr : System.Address := System.Null_Address;
      P_Impl          : DDS.Publisher_Impl.Ref_Access;
      P_Impl_A        : Ref_Access_Access;

      Need_To_Enable : aliased DDS.Boolean := False;
      Created        : Boolean := False;

      use type DDS.PublisherListener.Ref_Access;

   begin

      C_Publisher_Ptr := C_Create_Disabled
        (Participant => C_Participant,
         Need_Enable => Need_To_Enable'Unchecked_Access,
         Qos         => Qos.GetInterface,
         C_Listener  => System.Null_Address, --  re-set after Ada object created and before enable
         Mask        => Mask);

      if C_Publisher_Ptr /= System.Null_Address then
         P_Impl := new DDS.Publisher_Impl.Ref;
         P_Impl.Entity_Initialize_I (C_Publisher_Ptr);

         P_Impl_A :=  C_Publisher_Get_User_Data_I (C_Publisher_Ptr);
         if P_Impl_A /= null then
            P_Impl_A.all := P_Impl;
            --  Set Up Listener
            if A_Listener /=  null then
               P_Impl.Set_Listener (A_Listener, Mask);
            end if;
            if Need_To_Enable  then
               P_Impl.Enable;
               Created := True;
            end if;
         end if;
      end if;

      if Created  then
         return DDS.Publisher.Ref_Access (P_Impl);
      else
         raise DDS_ERROR with "Unable to create publisher";
      end if;

   end CreateI;

   function Get_FacadeI (C_Publisher : System.Address)
                        return Ref_Access is
      P_Impl : Ref_Access := null;
      P_Impl_Access : Ref_Access_Access;
   begin

      if C_Publisher /= System.Null_Address then
         P_Impl_Access :=
           C_Publisher_Get_User_Data_I (C_Publisher);
         if P_Impl_Access /= null then
            P_Impl := P_Impl_Access.all;
         end if;
      end if;

      return P_Impl;

   end Get_FacadeI;

end DDS.Publisher_Impl;
