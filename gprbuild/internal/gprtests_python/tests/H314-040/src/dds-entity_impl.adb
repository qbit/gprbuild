pragma Ada_05;

with System;

with DDS.Condition;

package body DDS.Entity_Impl is

   procedure Enable (Self : not null access Ref) is
      function Internal (Self : System.Address)
        return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_Entity_enable");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface));
   end Enable;

   function Get_StatusCondition (Self : not null access Ref) return
     DDS.StatusCondition.Ref_Access
   is
   begin
      return Self.StatusCondition'Access;
   end Get_StatusCondition;

   function Get_Status_Changes (Self : not null access Ref) return
     DDS.StatusMask
   is
      function Internal (Self : System.Address)
                        return DDS.StatusMask;
      pragma Import (C, Internal, "DDS_Entity_get_status_changes");
   begin
      return Internal (Self.GetInterface);
   end Get_Status_Changes;

   function Get_Instance_Handle (Self : not null access Ref) return
     DDS.InstanceHandle_T
   is
      function Internal (Self : System.Address)
                        return DDS.InstanceHandle_T;
      pragma Import (C, Internal, "DDS_Entity_get_instance_handle");
   begin
      return Internal (Self.GetInterface);
   end Get_Instance_Handle;

   procedure Entity_Initialize_I (Self : access Ref;
                                  CEntity : System.Address)
   is
      CStatusCondition : System.Address;

      function Get_CStatusCondition (CEntity : System.Address)
                                    return System.Address;
      pragma Import (C, Get_CStatusCondition, "DDS_Entity_get_statuscondition");

      procedure Set_User_Object (CStatusCondition : System.Address;
                                 UserObject : DDS.Condition.Ref_Access);
      pragma Import (C, Set_User_Object, "DDS_Condition_set_user_objectI");
   begin
      Self.SetInterface (CEntity);
      CStatusCondition := Get_CStatusCondition (CEntity);
      Self.StatusCondition.SetInterface (CStatusCondition);

      Set_User_Object (CStatusCondition, Self.StatusCondition'Access);
   end Entity_Initialize_I;

end DDS.Entity_Impl;

