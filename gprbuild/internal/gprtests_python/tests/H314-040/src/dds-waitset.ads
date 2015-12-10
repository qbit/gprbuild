pragma Ada_05;

with Ada.Finalization;
limited with DDS.WaitSet_Impl;
with DDS.ConditionSeq;
with DDS.Condition;

package DDS.WaitSet is

   type Ref is new Ada.Finalization.Controlled with private;
   type Ref_Access is access all Ref'Class;

   procedure Initialize
     (Self : in out Ref);

   procedure Adjust
     (Self : in out Ref);

   procedure Finalize
     (Self : in out Ref);

   procedure Wait
     (Self              : not null access Ref;
      Active_Conditions : access DDS.ConditionSeq.Sequence;
      Timeout           : in DDS.Duration_T);

   procedure Attach_Condition
     (Self : not null access Ref;
      Cond : access DDS.Condition.Ref'Class);

   procedure Detach_Condition
     (Self : not null access Ref;
      Cond : access DDS.Condition.Ref'Class);

   procedure Get_Conditions
     (Self                : not null access Ref;
      Attached_Conditions : access DDS.ConditionSeq.Sequence);

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.WaitSet_Impl.Ref;

   procedure Free (This : in out Ref_Access);

private

   type Ref is new Ada.Finalization.Controlled with
      record
         Impl_Access : access DDS.WaitSet_Impl.Ref;
      end record;

   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

end DDS.WaitSet;
