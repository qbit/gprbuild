pragma Ada_05;

with System;

package body DDS.StatusCondition_Impl is

   function Get_Enabled_Statuses
     (Self : not null access Ref)
     return DDS.StatusMask
   is
      function Internal
        (Self : System.Address)
        return DDS.StatusMask;
      pragma Import (C, Internal, "DDS_StatusCondition_get_enabled_statuses");
   begin
      return Internal (Self.GetInterface);
   end Get_Enabled_Statuses;

   function Get_Trigger_Value
     (Self : access Ref)
     return DDS.Boolean
   is
      function Internal
           (Self : System.Address)
        return DDS.Boolean;
         pragma Import (C, Internal, "DDS_Condition_get_trigger_value");
   begin
      return Internal (Self.GetInterface);
   end Get_Trigger_Value;

   procedure Set_Enabled_Statuses
     (Self : not null access Ref;
      Mask : in DDS.StatusMask)
   is
      function Internal
        (Self : System.Address;
         Mask : DDS.StatusMask)
        return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_StatusCondition_set_enabled_statuses");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Mask));
   end Set_Enabled_Statuses;

   function Get_Entity
     (Self : not null access Ref)
      return access DDS.Entity.Ref'Class
   is
   begin
      return Self.Owner;
   end Get_Entity;

   function Get_Impl_I
     (Self : access Ref)
      return access DDS.Condition_Impl.Ref
   is
   begin
      return DDS.Condition_Impl.Ref_Access (Self);
   end Get_Impl_I;

end DDS.StatusCondition_Impl;
