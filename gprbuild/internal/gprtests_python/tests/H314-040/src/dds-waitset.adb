pragma Ada_05;

with DDS.WaitSet_Impl;

package body DDS.WaitSet is

   procedure Initialize
     (Self : in out Ref)
   is
   begin
      Self.Impl_Access := new DDS.WaitSet_Impl.Ref;
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
      DDS.WaitSet_Impl.Free
        (DDS.WaitSet_Impl.Ref_Access (Self.Impl_Access));
   end Finalize;

   procedure Wait
     (Self              : not null access Ref;
      Active_Conditions : access DDS.ConditionSeq.Sequence;
      Timeout           : in DDS.Duration_T)
   is
   begin
      Self.Impl_Access.Wait (Active_Conditions, Timeout);
   end Wait;

   procedure Attach_Condition
     (Self : not null access Ref;
      Cond : access DDS.Condition.Ref'Class)
   is
   begin
      Self.Impl_Access.Attach_Condition (Cond);
   end Attach_Condition;

   procedure Detach_Condition
     (Self : not null access Ref;
      Cond : access DDS.Condition.Ref'Class)
   is
   begin
      Self.Impl_Access.Detach_Condition (Cond);
   end Detach_Condition;

   procedure Get_Conditions
     (Self                : not null access Ref;
      Attached_Conditions : access DDS.ConditionSeq.Sequence)
   is
   begin
      Self.Impl_Access.Get_Conditions (Attached_Conditions);
   end Get_Conditions;

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.WaitSet_Impl.Ref
   is
   begin
      return Self.Impl_Access;
   end Get_Impl_I;

end DDS.WaitSet;
