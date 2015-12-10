
pragma Ada_05;


package body DDS.ContentFilteredTopic_Impl is

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

   ---------------------
   -- get_participant --
   ---------------------

   function Get_Participant
     (Self : not null access Ref)
      return access DDS.DomainParticipant.Ref'Class
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return null;
   end Get_Participant;


   --


   ---------------------------
   -- get_filter_expression --
   ---------------------------

   function Get_Filter_Expression
     (Self : not null access Ref)
      return DDS.String
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return Get_Filter_Expression (Self);
   end Get_Filter_Expression;

   -------------------------------
   -- get_expression_parameters --
   -------------------------------

--     function Get_Expression_Parameters
--       (Self : not null access Ref)
--        return DDS.StringSeq
--     is
--     begin
--        return Get_Expression_Parameters (Self);
--     end Get_Expression_Parameters;

   -------------------------------
   -- set_expression_parameters --
   -------------------------------

--     function Set_Expression_Parameters
--       (Self                  : not null access Ref;
--        Expression_Parameters : in DDS.StringSeq)
--        return DDS.ReturnCode_T
--     is
--     begin
--        return Set_Expression_Parameters (Self, Expression_Parameters);
--     end Set_Expression_Parameters;

   -----------------------
   -- get_related_topic --
   -----------------------

   function Get_Related_Topic
     (Self : not null access Ref)
      return DDS.Topic.Ref_Access
   is
   begin
      return null;
   end Get_Related_Topic;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String)
      return DDS.Boolean
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return false;
   end Is_A;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Logical_Type_Id : Standard.String)
      return DDS.Boolean
   is
   begin
      Ret_Code_To_Exception (DDS.RETCODE_UNSUPPORTED, "Not Implemented");
      return false;
   end Is_A;

end DDS.ContentFilteredTopic_Impl;
