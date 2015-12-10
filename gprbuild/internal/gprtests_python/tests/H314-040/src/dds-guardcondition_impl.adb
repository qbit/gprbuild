pragma Ada_05;

with System;

package body DDS.GuardCondition_Impl is

   procedure Initialize
     (Self : in out Ref)
   is
      function Internal (Self : Ref_Access) return System.Address;
      pragma Import (C, Internal, "DDS_GuardCondition_newI");
   begin
      Self.SetInterface (Internal (Self'Unrestricted_Access));
   end Initialize;

   procedure Adjust
     (Self : in out Ref)
   is
   begin
      null;
   end Adjust;

   procedure Finalize
     (Self : in out Ref)
   is
      procedure Internal (CGuardCondition : System.Address);
      pragma Import (C, Internal, "DDS_GuardCondition_delete");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Set_Trigger_Value
     (Self : access Ref;
      Value : DDS.Boolean)
   is
      function Internal (CGuardCondition : System.Address;
                         Value           : DDS.Boolean)
        return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_GuardCondition_set_trigger_value");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Value));
   end Set_Trigger_Value;

   function Get_Trigger_Value
     (Self : access Ref)
     return DDS.Boolean
   is
      function Internal (CGuardCondition : System.Address)
        return DDS.Boolean;
      pragma Import (C, Internal, "DDS_Condition_get_trigger_value");
   begin
      return Internal (Self.GetInterface);
   end Get_Trigger_Value;

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.Condition_Impl.Ref
   is
   begin
      return DDS.Condition_Impl.Ref_Access (Self);
   end Get_Impl_I;

end DDS.GuardCondition_Impl;
