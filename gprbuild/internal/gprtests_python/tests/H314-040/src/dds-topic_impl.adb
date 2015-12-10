pragma Ada_05;

with DDS.Listener;
with DDS.Listener.Low_Level;
with DDS.TopicListener.Low_Level;

with Unchecked_Conversion;

package body DDS.Topic_Impl is

   use type System.Address;

   type C_TopicWrapper is record
      As_Entity           : System.Address;
      As_TopicDescription : System.Address;
      C_Impl              : System.Address;
   end record;
   pragma Convention (C, C_TopicWrapper);
   type C_TopicWrapper_Access is access C_TopicWrapper;

   function A_To_TW is new Unchecked_Conversion
     (Source => System.Address, Target => C_TopicWrapper_Access);

   type Ref_Access_Access is access all Ref_Access;
   function C_Topic_Get_User_Data_I (C_Topic : System.Address)
                                     return Ref_Access_Access;
   pragma Import (C, C_Topic_Get_User_Data_I,
                  "DDS_Entity_get_user_dataI");

   --  Re-Implement fron DDS.TopicDescription--

   -------------------
   -- get_type_name --
   -------------------

   function Get_Type_Name (Self : not null access Ref) return DDS.String is
      function Internal (Self : System.Address) return DDS.String;
      pragma Import (C, Internal, "DDS_TopicDescription_get_type_name");
   begin
      return Internal (Self.GetInterface);
   end Get_Type_Name;


   --------------
   -- get_name --
   --------------

   function Get_Name
     (Self : not null access Ref)
      return DDS.String is
      function Internal (Self : System.Address) return DDS.String;
      pragma Import (C, Internal, "DDS_TopicDescription_get_name");
   begin
      return Internal (Self.GetInterface);
   end Get_Name;


   --


   ---------------------
   -- get_participant --
   ---------------------

   function Get_Participant
     (Self : not null access Ref)
      return access DDS.DomainParticipant.Ref'Class
   is
   begin
      return Get_Participant (Self);
   end Get_Participant;

   -------------
   -- Set_Qos --
   -------------

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.TopicQos) is
      function Internal
        (Self  : System.Address;
         Qos   : System.Address)
         return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Topic_set_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Qos;

   -------------
   -- Get_Qos --
   -------------

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.TopicQos) is
      function Internal
        (Self  : System.Address;
         Qos   : System.Address)
         return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Topic_get_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Qos;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.TopicListener.Ref_Access;
      Mask       : in DDS.StatusMask) is
      function C_Set_Listener (C_Topic    : in System.Address;
                               C_Listener : in System.Address;
                               Mask       : in DDS.StatusMask)
                               return DDS.ReturnCode_T;
      pragma Import (C, C_Set_Listener, "DDS_Topic_set_listener");

      C_Listener : DDS.TopicListener.Low_Level.C_TopicListener :=
                     DDS.TopicListener.Low_Level.C_TopicListener_DEFAULT;

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
      return DDS.TopicListener.Ref_Access is

      procedure C_Get_Listener (C_Topic    : in System.Address;
                                C_Listener : in System.Address);
      pragma Import (C, C_Get_Listener,
                     "DDS_Topic_get_listenerX");

      C_Listener : DDS.TopicListener.Low_Level.C_TopicListener;

   begin
      C_Get_Listener (Self.GetInterface, C_Listener'Address);
      return DDS.TopicListener.Ref_Access
        (DDS.Listener.Low_Level.C_To_Listener (C_Listener.Listener));
   end Get_Listener;

   -----------------------------------
   -- Get_Inconsistent_Topic_Status --
   -----------------------------------

   procedure Get_Inconsistent_Topic_Status
     (Self     : not null access Ref;
      A_Status : in out DDS.InconsistentTopicStatus;
      Returns  : out DDS.ReturnCode_T)
   is
   begin
      null;
   end Get_Inconsistent_Topic_Status;

   -----------------------------------
   -- Set_Inconsistent_Topic_Status --
   -----------------------------------

   procedure Set_Inconsistent_Topic_Status
     (Self     : not null access Ref;
      A_Status : in DDS.InconsistentTopicStatus;
      Returns  : out DDS.ReturnCode_T)
   is
   begin
      null;
   end Set_Inconsistent_Topic_Status;


   function Get_TopicDescription (Self : not null access Ref)
                                  return DDS.TopicDescription.Ref_Access is

   begin
      return null;
   end Get_TopicDescription;

   function As_C_TopicDescriptionI (C_Topic : System.Address)
                                    return System.Address is
      C_Topic_Ptr : constant C_TopicWrapper_Access := A_To_TW (C_Topic);
   begin
      return C_Topic_Ptr.As_TopicDescription;
   end As_C_TopicDescriptionI;

   function As_C_EntityI (C_Topic : System.Address)
                          return System.Address is
      C_Topic_Ptr : constant C_TopicWrapper_Access := A_To_TW (C_Topic);
   begin
      return C_Topic_Ptr.As_Entity;
   end As_C_EntityI;

   function CreateI
     (C_Participant : System.Address;
      Topic_Name    : in DDS.String;
      Type_Name     : in DDS.String;
      Qos           : in DDS.TopicQos;
      A_Listener    : in DDS.TopicListener.Ref_Access;
      Mask          : in DDS.StatusMask)
      return DDS.Topic.Ref_Access is

      function C_Create_Disabled (Participant : System.Address;
                                  Need_Enable : DDS.Boolean_Ptr;
                                  Topic_Name  : DDS.String;
                                  Type_Name   : DDS.String;
                                  Qos         : System.Address;
                                  A_Listener  : System.Address;
                                  Mask        : DDS.StatusMask)
                                  return System.Address;
      pragma Import (C, C_Create_Disabled,
                     "DDS_DomainParticipant_create_topic_disabledI");

      function C_Enable (Topic : System.Address) return DDS.ReturnCode_T;
      pragma Import (C, C_Enable, "DDS_Entity_enable");

      C_Topic_Ptr     : System.Address := System.Null_Address;
      T_Impl          : DDS.Topic_Impl.Ref_Access;
      T_Impl_A        : Ref_Access_Access;

      Need_To_Enable : aliased DDS.Boolean := False;
      Created        : Boolean := False;

      Rc : DDS.ReturnCode_T;

      use type DDS.TopicListener.Ref_Access;

      function Internal (Self       : System.Address;
                         Topic_Name : DDS.String;
                         Type_Name  : DDS.String;
                         Qos        : System.Address;
                         A_Listener : System.Address;
                         Mask       : DDS.StatusMask) return System.Address;
      pragma Import (C, Internal, "DDS_DomainParticipant_create_topic");

   begin

      --        C_Topic_Ptr := Internal (Self       => C_Participant,
      --                                 Topic_Name => Topic_Name,
      --                                 Type_Name  => Type_Name,
      --                                Qos        => Qos.GetInterface,
      --                                A_Listener => C_Listener'Address,
      --                                Mask       => Mask);
      C_Topic_Ptr := C_Create_Disabled
        (Participant => C_Participant,
         Need_Enable => Need_To_Enable'Unchecked_Access,
         Topic_Name  => Topic_Name,
         Type_Name   => Type_Name,
         Qos         => Qos.GetInterface,
         A_Listener  => System.Null_Address,
         Mask        => Mask);

      if C_Topic_Ptr /= System.Null_Address then
         T_Impl := new DDS.Topic_Impl.Ref;

         --  T_Impl.Entity_Initialize_I (C_Topic_Ptr);
         T_Impl.SetInterface (C_Topic_Ptr);

         T_Impl_A :=  C_Topic_Get_User_Data_I (As_C_EntityI (C_Topic_Ptr));
         if T_Impl_A /= null then
            T_Impl_A.all := T_Impl;
            --  Set Up Listener
            if A_Listener /=  null then
               T_Impl.Set_Listener (A_Listener, Mask);
            end if;
            if Need_To_Enable
            then
               --T_Impl.Enable;
               Rc := C_Enable ( As_C_EntityI (C_Topic_Ptr));
            end if;
            Created := True;
         end if;
      end if;

      if Created then
         return DDS.Topic.Ref_Access (T_Impl);
      else
         raise DDS_ERROR with "Unable to Create Topic";
      end if;

   exception
      when others =>
         raise DDS_ERROR with "Unable to Create Topic";
   end CreateI;

   function Get_FacadeI (C_Topic : System.Address)
                         return Ref_Access is
      P_Impl        : Ref_Access := null;
      P_Impl_Access : Ref_Access_Access;
   begin

      if C_Topic /= System.Null_Address then
         P_Impl_Access :=
           C_Topic_Get_User_Data_I (As_C_EntityI (C_Topic));
         if P_Impl_Access /= null then
            P_Impl := P_Impl_Access.all;
         end if;
      end if;

      return P_Impl;

   end Get_FacadeI;


end DDS.Topic_Impl;
