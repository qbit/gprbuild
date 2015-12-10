
pragma Ada_05;

package body DDS.MultiTopic_Impl is

      --  Re-Implement From DDS.Entity

   procedure Enable (Self : not null access Ref) is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
   end Enable;

   function Get_StatusCondition (Self : not null access Ref) return
     DDS.StatusCondition.Ref_Access is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return null;
   end Get_StatusCondition;

   function Get_Status_Changes (Self : not null access Ref) return
     DDS.StatusMask is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.STATUS_MASK_NONE;
   end Get_Status_Changes;

   function Get_Instance_Handle (Self : not null access Ref) return
     DDS.InstanceHandle_T is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return DDS.Null_InstanceHandle_T;
   end Get_Instance_Handle;
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
   ---------------------------------
   -- get_subscription_expression --
   ---------------------------------

   function get_subscription_expression
     (Self : not null access Ref)
      return DDS.String
   is
   begin
      return get_subscription_expression (Self);
   end get_subscription_expression;

   -------------------------------
   -- get_expression_parameters --
   -------------------------------

--     function get_expression_parameters
--       (Self : not null access Ref)
--        return DDS.StringSeq
--     is
--     begin
--        return get_expression_parameters (Self);
--     end get_expression_parameters;
--
--     -------------------------------
--     -- set_expression_parameters --
--     -------------------------------
--
--     function set_expression_parameters
--       (Self : not null access Ref;
--        expression_parameters : in DDS.StringSeq)
--        return DDS.ReturnCode_t
--     is
--     begin
--        return set_expression_parameters (Self, expression_parameters);
--     end set_expression_parameters;

   ----------
   -- Is_A --
   ----------


end DDS.MultiTopic_Impl;
