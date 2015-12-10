 pragma Ada_05;

with DDS.DataReader_Impl;
with DDS.Listener;
with DDS.Listener.Low_Level;
with DDS.SubscriberListener.Low_Level;

with System;


package body DDS.Subscriber_Impl is

   use type System.Address;

   type Ref_Access_Access is access all Ref_Access;
   function C_Subscriber_Get_User_Data_I (C_Subscriber : System.Address)
                                         return Ref_Access_Access;
   pragma Import (C, C_Subscriber_Get_User_Data_I,
                    "DDS_Entity_get_user_dataI");


   -----------------------
   -- Create_DataReader --
   -----------------------
   function Create_DataReader
     (Self       : not null access Ref;
      A_Topic    : not null DDS.Topic.Ref_Access;
      Qos        : DDS.DataReaderQos;
      A_Listener : in DDS.DataReaderListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.DataReader.Ref_Access is
   begin
      return DDS.DataReader_Impl.CreateI (Self.GetInterface,
                                   A_Topic,
                                   Qos,
                                   A_Listener,
                                   Mask);
   end Create_DataReader;

   -----------------------
   -- delete_datareader --
   -----------------------
   procedure Delete_DataReader
     (Self         : not null access Ref;
      A_DataReader : in out DDS.DataReader.Ref_Access)
   is
      function Internal
        (Self        : System.Address;
         Reader       : System.Address)
         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_Subscriber_delete_datareader");

      R : DDS.DataReader_Impl.Ref_Access :=
            DDS.DataReader_Impl.Ref_Access (A_DataReader);

      use type DDS.DataReader.Ref_Access;

   begin
      if A_DataReader /= null then
         Ret_Code_To_Exception
           (Internal (Self.GetInterface, R.GetInterface));
         DataReader_Impl.Free (R);
         A_DataReader := DDS.DataReader.Ref_Access (R);
      end if;
   end Delete_DataReader;

   -------------------------------
   -- delete_contained_entities --
   -------------------------------
   procedure Delete_Contained_Entities
     (Self : not null access Ref)
   is
      function Internal (Self : System.Address) return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_Subscriber_delete_contained_entities");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface), "Unable to Delete enteties");
   end Delete_Contained_Entities;

   -----------------------
   -- lookup_datareader --
   -----------------------
   function Lookup_DataReader
     (Self       : not null access Ref;
      Topic_Name : not null DDS.String_Ptr)
      return DDS.DataReader.Ref_Access
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return Lookup_DataReader (Self, Topic_Name);
   end Lookup_DataReader;

   ---------------------
   -- get_datareaders --
   ---------------------
   procedure Get_DataReaders
     (Self            : not null access Ref;
      Readers         : not null DDS.DataReaderSeq.Ref_Access;
      Sample_States   : in DDS.SampleStateKind_Access;
      View_States     : in DDS.ViewStateKind_Access;
      Instance_States : in DDS.InstanceStateKind_Access) is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Get_DataReaders;

   ------------------------
   -- notify_datareaders --
   ------------------------
   procedure Notify_DataReaders
     (Self : not null access Ref) is
      function C_Notify_DataReaders (C_Subscriber : System.Address) return
        DDS.ReturnCode_T;
      pragma Interface (C, C_Notify_DataReaders,
                          "DDS_Subscriber_notify_datareaders");
   begin
      Ret_Code_To_Exception (C_Notify_DataReaders (Self.GetInterface),
                             "Notify_DataReaders FAILED");
   end Notify_DataReaders;

   -------------
   -- set_qos --
   -------------
   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.SubscriberQos)
   is
      function Internal (Self  : System.Address;
                         Qos   : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Subscriber_set_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Qos;

   -------------
   -- get_qos --
   -------------
   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.SubscriberQos)
   is
      function Internal (Self  : System.Address;
                         Qos   : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Subscriber_get_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Qos;

   ------------------
   -- set_listener --
   ------------------
   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : not null DDS.SubscriberListener.Ref_Access;
      Mask       : in DDS.StatusKind)
   is
      function C_Set_Listener (C_Subscriber : in System.Address;
                               C_Listener   : in System.Address;
                               Mask         : in DDS.StatusMask)
        return DDS.ReturnCode_T;
      pragma Import (C, C_Set_Listener, "DDS_Subscriber_set_listener");

      --  All callbacks initialized in declaration
      C_Listener : DDS.SubscriberListener.Low_Level.C_SubscriberListener :=
        DDS.SubscriberListener.Low_Level.C_SubscriberListener_DEFAULT;

   begin
      C_Listener.As_DataReaderListener.Listener :=
        DDS.Listener.Low_Level.Listener_To_C (DDS.Listener.Ref_Access (A_Listener));
      if Self.GetInterface /= System.Null_Address then
         Ret_Code_To_Exception (C_Set_Listener (Self.GetInterface,
                                                C_Listener'Address,
                                                Mask),
                                "Set Listener FAILED");
      end if;
   end Set_Listener;

   ------------------
   -- get_listener --
   ------------------
   function Get_Listener
     (Self : not null access Ref)
      return DDS.SubscriberListener.Ref_Access is

      C_Listener : DDS.SubscriberListener.Low_Level.C_SubscriberListener;

      procedure C_Get_Listener (C_Subscriber : in System.Address;
                                C_Listener   : in System.Address);
      pragma Import (C, C_Get_Listener,
                       "DDS_Subscriber_get_listenerX");
   begin
      C_Get_Listener (Self.GetInterface, C_Listener'Address);
      return DDS.SubscriberListener.Ref_Access (
        DDS.Listener.Low_Level.C_To_Listener (C_Listener.As_DataReaderListener.Listener));
   end Get_Listener;

   ------------------
   -- begin_access --
   ------------------
   procedure Begin_Access
     (Self : not null access Ref)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Begin_Access;

   ----------------
   -- end_access --
   ----------------
   procedure End_Access
     (Self : not null access Ref)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end End_Access;

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

   --------------------------------
   -- set_default_datareader_qos --
   --------------------------------
   procedure Set_Default_DataReader_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataReaderQos)
   is
      function Internal (Self  : System.Address;
                         Qos   : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Subscriber_set_default_datareader_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Default_DataReader_Qos;

   --------------------------------
   -- get_default_datareader_qos --
   --------------------------------
   procedure Get_Default_DataReader_Qos
     (Self  : not null access Ref;
      Qos   : in out DDS.DataReaderQos)
   is
      function Internal (Self  : System.Address;
                         Qos   : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Subscriber_get_default_datareader_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Default_DataReader_Qos;

   -------------------------
   -- copy_from_topic_qos --
   -------------------------
   procedure Copy_From_Topic_Qos
     (Self               : not null access Ref;
      A_DataReader_Qos   : in out DDS.DataReaderQos;
      A_Topic_Qos        : in DDS.TopicQos) is
      function Internal (Self   : System.Address;
                         D_Qos  : System.Address;
                         T_Qos  : System.Address)
                        return ReturnCode_T;
      pragma Import (C, Internal, "DDS_Subscriber_copy_from_topic_qos");
   begin
      Ret_Code_To_Exception
        (Internal (Self.GetInterface,
                   A_DataReader_Qos.GetInterface,
                   A_Topic_Qos.GetInterface));
   end Copy_From_Topic_Qos;

   function CreateI
     (C_Participant : in System.Address;
      Qos           : in DDS.SubscriberQos;
      A_Listener    : in DDS.SubscriberListener.Ref_Access;
      Mask          : in DDS.StatusMask)
     return DDS.Subscriber.Ref_Access is

      function C_Create_Disabled (Participant : System.Address;
                                  Need_Enable : DDS.Boolean_Ptr;
                                  Qos         : System.Address;
                                  C_Listener  : System.Address;
                                  Mask        : DDS.StatusMask)
                                 return System.Address;
      pragma Import (C, C_Create_Disabled,
                       "DDS_DomainParticipant_create_subscriber_disabledI");

      C_Subscriber_Ptr : System.Address := System.Null_Address;
      S_Impl           : Ref_Access;
      S_Impl_A         : Ref_Access_Access;

      Need_To_Enable : aliased DDS.Boolean := False;
      Created        : Boolean := False;

      use type DDS.SubscriberListener.Ref_Access;

   begin

      C_Subscriber_Ptr := C_Create_Disabled
        (Participant => C_Participant,
         Need_Enable => Need_To_Enable'Unchecked_Access,
         Qos         => Qos.GetInterface,
         C_Listener  => System.Null_Address, --  will re-set after Ada object created and before enable
         Mask        => Mask);

      if C_Subscriber_Ptr /= System.Null_Address then
         S_Impl := new DDS.Subscriber_Impl.Ref;
         S_Impl.Entity_Initialize_I (C_Subscriber_Ptr);

         --  Set Listener
         S_Impl_A :=  C_Subscriber_Get_User_Data_I (C_Subscriber_Ptr);
         if S_Impl_A /= null then
            S_Impl_A.all := S_Impl;

            --  Set up Listener
            if A_Listener /=  null then
               S_Impl.Set_Listener (A_Listener, Mask);
            end if;

            if Need_To_Enable  then
               S_Impl.Enable;
               Created := True;
            end if;
         end if;
      end if;

      if Created  then
         return DDS.Subscriber.Ref_Access (S_Impl);
      else
         raise DDS_ERROR with "Unable to create Subscriber";
      end if;

   end CreateI;

   function Get_FacadeI (C_Subscriber : System.Address)
                        return Ref_Access is
      P_Impl : Ref_Access := null;
      P_Impl_Access : Ref_Access_Access;
   begin

      if C_Subscriber /= System.Null_Address then
         P_Impl_Access :=
           C_Subscriber_Get_User_Data_I (C_Subscriber);
         if P_Impl_Access /= null then
            P_Impl := P_Impl_Access.all;
         end if;
      end if;

      return P_Impl;

   end Get_FacadeI;


end DDS.Subscriber_Impl;
