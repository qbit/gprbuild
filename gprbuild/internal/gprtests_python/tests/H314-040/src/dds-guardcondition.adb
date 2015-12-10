pragma Ada_05;

with DDS.GuardCondition_Impl;

package body DDS.GuardCondition is

   procedure Initialize
     (Self : in out Ref)
   is
   begin
      Self.Impl_Access := new DDS.GuardCondition_Impl.Ref;
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
   begin
      DDS.GuardCondition_Impl.Free
        (DDS.GuardCondition_Impl.Ref_Access (Self.Impl_Access));
   end Finalize;

   procedure Set_Trigger_Value
     (Self : access Ref;
      Value : DDS.Boolean)
   is
   begin
      Self.Impl_Access.Set_Trigger_Value (Value);
   end Set_Trigger_Value;

   function Get_Trigger_Value
     (Self : access Ref)
     return DDS.Boolean
   is
   begin
      return Self.Impl_Access.Get_Trigger_Value;
   end Get_Trigger_Value;

   function Get_Impl_I
     (Self : access Ref)
      return access DDS.Condition_Impl.Ref
   is
   begin
      return DDS.Condition_Impl.Ref_Access (Self.Impl_Access);
   end Get_Impl_I;

end DDS.GuardCondition;
