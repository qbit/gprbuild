pragma Ada_05;

with DDS.Listener;
with DDS.Listener.Low_Level;
with DDS.DomainParticipantListener.Low_Level;
with DDS.Topic_Impl;
with DDS.Subscriber_Impl;
with DDS.Publisher_Impl;

with System;

package body DDS.DomainParticipant_Impl is
   use DDS.Topic;
   use DDS.Subscriber;
   use DDS.Publisher;

   use System;

   type Ref_Access_Access is access all Ref_Access;
   function C_DDS_Entity_Get_User_Data_I (C_Participant : System.Address)
                                         return Ref_Access_Access;
   pragma Import (C, C_DDS_Entity_Get_User_Data_I,
                    "DDS_Entity_get_user_dataI");

   --  Allow storage for access type
   Default_Impl_Size : constant DDS.Long := ((System.Address'Size + 7) / 8);
   --  Always align on 64-bit boundaries
   Default_Impl_Alignment : constant DDS.Long := 8;

   --
   function Create_Publisher
     (Self       : not null access Ref;
      Qos        : in DDS.PublisherQos;
      A_Listener : in DDS.PublisherListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Publisher.Ref_Access is
   begin

      return (DDS.Publisher_Impl.CreateI
              (Self.GetInterface,
               Qos,
               A_Listener,
               Mask));

   end Create_Publisher;

   ----------------------
   -- Delete_Publisher --
   ----------------------

   procedure Delete_Publisher
     (Self      : not null access Ref;
      Publisher : in out DDS.Publisher.Ref_Access)
   is
      function Internal (Self      : System.Address;
                         Publisher : System.Address) return  DDS.ReturnCode_T;

      pragma Import (C, Internal, "DDS_DomainParticipant_delete_publisher");

      P : DDS.Publisher_Impl.Ref_Access :=
            DDS.Publisher_Impl.Ref_Access (Publisher);

   begin
      if Publisher /= null then
         Ret_Code_To_Exception (Internal (Self.GetInterface, P.GetInterface));
         DDS.Publisher_Impl.Free (P);
      end if;
   end Delete_Publisher;


   -----------------------
   -- Create_Subscriber --
   -----------------------

   function Create_Subscriber
     (Self       : not null access Ref;
      Qos        : in DDS.SubscriberQos;
      A_Listener : in DDS.SubscriberListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Subscriber.Ref_Access
   is
   begin

      return (DDS.Subscriber_Impl.CreateI
              (Self.GetInterface,
               Qos,
               A_Listener,
               Mask));

   end Create_Subscriber;

   -----------------------
   -- Delete_Subscriber --
   -----------------------

   procedure Delete_Subscriber
     (Self :  not null access Ref;
      S    :  in out DDS.Subscriber.Ref_Access)
   is
      function Internal (Self      : System.Address;
                         Publisher : System.Address) return  DDS.ReturnCode_T;

      pragma Import (C, Internal, "DDS_DomainParticipant_delete_subscriber");

      use type DDS.Subscriber_Impl.Ref_Access;

      S_Impl : DDS.Subscriber_Impl.Ref_Access :=
                DDS.Subscriber_Impl.Ref_Access (S);

   begin
      if S_Impl /= null then
         Ret_Code_To_Exception (Internal (Self.GetInterface, S_Impl.GetInterface));
         Subscriber_Impl.Free (S_Impl);
      end if;
   end Delete_Subscriber;

   ----------------------------
   -- Get_Builtin_Subscriber --
   ----------------------------

   function Get_Builtin_Subscriber
     (Self : not null access Ref)
      return DDS.Subscriber.Ref_Access
   is
   begin
      return null;
   end Get_Builtin_Subscriber;

   ------------------
   -- Create_Topic --
   ------------------

   function Create_Topic
     (Self       : not null access Ref;
      Topic_Name : in DDS.String;
      Type_Name  : in DDS.String;
      Qos        : in DDS.TopicQos;
      A_Listener : in DDS.TopicListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.Topic.Ref_Access is
   begin
      return (DDS.Topic_Impl.CreateI (Self.GetInterface,
                                      Topic_Name,
                                      Type_Name,
                                      Qos,
                                      A_Listener,
                                      Mask));
   end Create_Topic;

   ------------------
   -- Delete_Topic --
   ------------------

   procedure Delete_Topic
     (Self    : not null access Ref;
      A_Topic : in out DDS.Topic.Ref_Access)
   is
      function Internal (Self      : System.Address;
                         Publisher : System.Address) return  DDS.ReturnCode_T;

      pragma Import (C, Internal, "DDS_DomainParticipant_delete_topic");

      T : DDS.Topic_Impl.Ref_Access :=
            DDS.Topic_Impl.Ref_Access (A_Topic);

   begin
      if A_Topic /= null then
         Ret_Code_To_Exception (Internal (Self.GetInterface, T.GetInterface));
         Topic_Impl.Free (T);
         A_Topic := DDS.Topic.Ref_Access (T);
      end if;
   end Delete_Topic;

   ----------------
   -- Find_Topic --
   ----------------

   function Find_Topic
     (Self       : not null access Ref;
      Topic_Name : in DDS.String;
      Timeout    : in DDS.Duration_T)
      return DDS.Topic.Ref_Access
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return null;
   end Find_Topic;

   -----------------------------
   -- Lookup_Topicdescription --
   -----------------------------

   function Lookup_Topicdescription
     (Self : not null access Ref;
      Name : in DDS.String)
      return DDS.TopicDescription.Ref_Access
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return null;
   end Lookup_Topicdescription;

   ---------------------------------
   -- Create_Contentfilteredtopic --
   ---------------------------------

--     function Create_Contentfilteredtopic
--       (Self              : access Ref;
--        Name              : in DDS.String;
--        Related_Topic     : access constant DDS.Topic.Ref'Class;
--        Filter_Expression : in DDS.String;
--        Filter_Parameters : in DDS.StringSeq)
--        return access DDS.ContentFilteredTopic.Ref'Class
--     is
--     begin
--        return Create_Contentfilteredtopic (Self, Name, Related_Topic,
--                                            Filter_Expression, Filter_Parameters);
--     end Create_Contentfilteredtopic;

   ---------------------------------
   -- Delete_Contentfilteredtopic --
   ---------------------------------

   procedure Delete_Contentfilteredtopic
     (Self                   : not null access Ref;
      A_Contentfilteredtopic : not null DDS.ContentFilteredTopic.Ref_Access)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Delete_Contentfilteredtopic;

   -----------------------
   -- Create_Multitopic --
   -----------------------

--     function Create_Multitopic
--       (Self                    : access Ref;
--        Name                    : in DDS.String;
--        Type_Name               : in DDS.String;
--        Subscription_Expression : in DDS.String;
--        Expression_Parameters   : in DDS.StringSeq)
--        return access DDS.MultiTopic.Ref'Class
--     is
--     begin
--        return Create_Multitopic (Self, Name, Type_Name, Subscription_Expression,
--                                  Expression_Parameters);
--     end Create_Multitopic;

   -----------------------
   -- Delete_Multitopic --
   -----------------------

   procedure Delete_Multitopic
     (Self         : not null access Ref;
      A_Multitopic : not null DDS.MultiTopic.Ref_Access)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Delete_Multitopic;

   -------------------------------
   -- Delete_Contained_Entities --
   -------------------------------

   procedure Delete_Contained_Entities
     (Self : not null access Ref)
   is
      function Internal (Self : System.Address) return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_delete_contained_entities");
      Ret : DDS.ReturnCode_T;
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface), "Unablento Delete enteties");
   end Delete_Contained_Entities;

   -------------
   -- Set_Qos --
   -------------

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DomainParticipantQos)
   is
      function Internal (Self : System.Address;
                         Qos :  System.Address)
                        return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_set_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Qos;

   -------------
   -- Get_Qos --
   -------------

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DomainParticipantQos)
   is
      function Internal (Self : System.Address;
                         Qos : System.Address)
                         return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_get_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Qos;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DomainParticipantListener.Ref_Access;
      Mask       : in DDS.StatusMask) is
      function C_Set_Listener (C_Participant : in System.Address;
                               C_Listener    : in System.Address;
                               Mask          : in DDS.StatusMask)
                              return DDS.ReturnCode_T;
      pragma Import (C, C_Set_Listener, "DDS_DomainParticipant_set_listener");

      --  All callbacks initialized in declaration
      C_Listener : DDS.DomainParticipantListener.Low_Level.C_DomainParticipantListener :=
        DDS.DomainParticipantListener.Low_Level.C_DomainParticipantListener_DEFAULT;
   begin
      C_Listener.As_TopicListener.Listener :=
        DDS.Listener.Low_Level.Listener_To_C (DDS.Listener.Ref_Access (A_Listener));
      C_Listener.As_PublisherListener.As_DataWriterListener.Listener :=
        DDS.Listener.Low_Level.Listener_To_C (DDS.Listener.Ref_Access (A_Listener));
      C_Listener.As_SubscriberListener.As_DataReaderListener.Listener :=
        DDS.Listener.Low_Level.Listener_To_C (DDS.Listener.Ref_Access (A_Listener));
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
     return DDS.DomainParticipantListener.Ref_Access is

      C_Listener : DDS.DomainParticipantListener.Low_Level.C_DomainParticipantListener;

      procedure C_Get_Listener (C_Participant : in System.Address;
                                C_Listener    : in System.Address);
      pragma Import (C, C_Get_Listener,
                       "DDS_DomainParticipant_get_listenerX");

   begin
      C_Get_Listener (Self.GetInterface, C_Listener'Address);
      return DDS.DomainParticipantListener.Ref_Access
        (DDS.Listener.Low_Level.C_To_Listener (C_Listener.As_TopicListener.Listener));
   end Get_Listener;

   ------------------------
   -- Ignore_Participant --
   ------------------------

   procedure Ignore_Participant
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Ignore_Participant;

   ------------------
   -- Ignore_Topic --
   ------------------

   procedure Ignore_Topic
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Ignore_Topic;

   ------------------------
   -- Ignore_Publication --
   ------------------------

   procedure Ignore_Publication
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Ignore_Publication;

   -------------------------
   -- Ignore_Subscription --
   -------------------------

   procedure Ignore_Subscription
     (Self   : not null access Ref;
      Handle : in DDS.InstanceHandle_T)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Ignore_Subscription;

   -------------------
   -- Get_Domain_Id --
   -------------------

   function Get_Domain_Id
     (Self : not null access Ref)
      return DDS.DomainId_T
   is
      function Internal (Self : System.Address
                        ) return  DDS.DomainId_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_assert_liveliness");
   begin
      return Internal (Self.GetInterface);
   end Get_Domain_Id;

   -----------------------
   -- Assert_Liveliness --
   -----------------------

   procedure Assert_Liveliness
     (Self : not null access Ref)
   is
      function Internal (Self : System.Address
                        ) return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_assert_liveliness");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface));
   end Assert_Liveliness;

   -------------------------------
   -- Set_Default_Publisher_Qos --
   -------------------------------

   procedure Set_Default_Publisher_Qos
     (Self : not null access Ref;
      Qos  : in DDS.PublisherQos)

   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                        return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_set_default_publisher_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Default_Publisher_Qos;

   -------------------------------
   -- Get_Default_Publisher_Qos --
   -------------------------------

   procedure Get_Default_Publisher_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.PublisherQos)
   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                        return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_get_default_publisher_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Default_Publisher_Qos;

   --------------------------------
   -- Set_Default_Subscriber_Qos --
   --------------------------------

   procedure Set_Default_Subscriber_Qos
     (Self : not null access Ref;
      Qos  : in DDS.SubscriberQos)
   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                        return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_set_default_subscriber_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Default_Subscriber_Qos;

   --------------------------------
   -- Get_Default_Subscriber_Qos --
   --------------------------------

   procedure Get_Default_Subscriber_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.SubscriberQos)
   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                         return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_get_default_subscriber_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Default_Subscriber_Qos;

   ---------------------------
   -- Set_Default_Topic_Qos --
   ---------------------------

   procedure Set_Default_Topic_Qos
     (Self : not null access Ref;
      Qos  : in DDS.TopicQos)
   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                        return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_set_default_topic_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Set_Default_Topic_Qos;

   ---------------------------
   -- Get_Default_Topic_Qos --
   ---------------------------

   procedure Get_Default_Topic_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.TopicQos)
   is
      function Internal (Self : System.Address;
                         Qos  : System.Address)
                        return  DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipant_get_default_topic_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));
   end Get_Default_Topic_Qos;

   ---------------------------------
   -- Get_Discovered_Participants --
   ---------------------------------

--     procedure Get_Discovered_Participants
--       (Self                : access Ref;
--        Participant_Handles : access DDS.InstanceHandleSeq;
--        Returns             : out DDS.ReturnCode_T)
--     is
--     begin
--        null;
--     end Get_Discovered_Participants;

   -------------------------------------
   -- Get_Discovered_Participant_Data --
   -------------------------------------

   procedure Get_Discovered_Participant_Data
     (Self               : not null access Ref;
      Participant_Handle : in DDS.InstanceHandle_T;
      Participant_Data   : not null DDS.ParticipantBuiltinTopicData_Access)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Get_Discovered_Participant_Data;

   ---------------------------
   -- Get_Discovered_Topics --
   ---------------------------

--     procedure Get_Discovered_Topics
--       (Self          : access Ref;
--        Topic_Handles : access DDS.InstanceHandleSeq;
--        Returns       : out DDS.ReturnCode_T)
--     is
--     begin
--        null;
--     end Get_Discovered_Topics;

   -------------------------------
   -- Get_Discovered_Topic_Data --
   -------------------------------

   procedure Get_Discovered_Topic_Data
     (Self         : not null access Ref;
      Topic_Handle : in DDS.InstanceHandle_T;
      Topic_Data   : not null DDS.TopicBuiltinTopicData_Access)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Get_Discovered_Topic_Data;

   ---------------------
   -- Contains_Entity --
   ---------------------

   function Contains_Entity
     (Self     : not null access Ref;
      A_Handle : in DDS.InstanceHandle_T)
      return Boolean
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return False;
   end Contains_Entity;

   ----------------------
   -- Get_Current_Time --
   ----------------------

   procedure Get_Current_Time
     (Self         : not null access Ref;
      Current_Time : in out DDS.Time_T)
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Get_Current_Time;

   function CreateI
     (C_Participant_Factory : in System.Address;
      Domain_Id             : in DDS.DomainId_T;
      Qos                   : in DDS.DomainParticipantQos;
      A_Listener            : in DDS.DomainParticipantListener.Ref_Access;
      Mask                  : in DDS.StatusMask)
     return DDS.DomainParticipant.Ref_Access is

      function Internal (Participant_Factory       : System.Address;
                         Need_Enable               : DDS.Boolean_Ptr;
                         Domain_Id                 : in DDS.DomainId_T;
                         Qos                       : in System.Address;
                         A_Listener                : System.Address;
                         Mask                      : in DDS.StatusMask;
                         Db_Thread_Factory         : System.Address;
                         Recv_Thread_Factory       : System.Address;
                         Event_Thread_Factory      : System.Address;
                         Synch_Pub_Thread_Factory  : System.Address;
                         Builtin_Types             : DDS.Boolean;
                         Sql_Filter                : DDS.Boolean)
                        return System.Address;
      pragma Import (C, Internal,
                     "DDS_DomainParticipantFactory_create_participant_disabledI");

      Need_To_Enable     : aliased DDS.Boolean := False;

      C_Participant      : System.Address;

      Created            : Boolean := False;
      P_Impl             : Ref_Access;
      P_Impl_Access      : Ref_Access_Access;

      Modified_Qos       : DDS.DomainParticipantQos;

      use type DDS.DomainParticipantListener.Ref_Access;

   begin

      Copy (Modified_Qos, Qos);
      Modified_Qos.User_Object.Participant_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Participant_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Topic_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Topic_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Content_Filtered_Topic_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Content_Filtered_Topic_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Flow_Controller_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Flow_Controller_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Publisher_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Publisher_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Data_Writer_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Data_Writer_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Subscriber_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Subscriber_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Data_Reader_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Data_Reader_User_Object.Alignment :=
        Default_Impl_Alignment;
      Modified_Qos.User_Object.Read_Condition_User_Object.Size :=
        Default_Impl_Size;
      Modified_Qos.User_Object.Read_Condition_User_Object.Alignment :=
        Default_Impl_Alignment;

      C_Participant := Internal (C_Participant_Factory,
                                 Need_To_Enable'Unchecked_Access,
                                 Domain_Id,
                                 Modified_Qos.GetInterface,
                                 System.Null_Address, -- will reset before enable
                                 Mask,
                                 System.Null_Address,
                                 System.Null_Address,
                                 System.Null_Address,
                                 System.Null_Address,
                                 False,
                                 False);

      if C_Participant /= System.Null_Address then
         P_Impl := new DomainParticipant_Impl.Ref;
         P_Impl.Entity_Initialize_I (C_Participant);
         P_Impl_Access := C_DDS_Entity_Get_User_Data_I (C_Participant);
         if P_Impl_Access /= null then
            P_Impl_Access.all := P_Impl;
            --  Set Listener
            if A_Listener /= null then
               P_Impl.Set_Listener (A_Listener, Mask);
            end if;
            if Need_To_Enable  then
               P_Impl.Enable;
            end if;
            Created := True;
         end if;
      end if;

      if Created  then
         return DomainParticipant.Ref_Access (P_Impl);
      else
         raise DDS_ERROR with "Unable to Create Participant";
      end if;

   end CreateI;

   function Get_FacadeI (C_DomainParticpant : System.Address)
                        return Ref_Access is

      P_Impl : Ref_Access := null;
      P_Impl_Access : Ref_Access_Access;

   begin

      if C_DomainParticpant /= System.Null_Address then
         P_Impl_Access :=
           C_DDS_Entity_Get_User_Data_I (C_DomainParticpant);
         if P_Impl_Access /= null then
            P_Impl := P_Impl_Access.all;
         end if;
      end if;

      return P_Impl;

   end Get_FacadeI;

end DDS.DomainParticipant_Impl;
