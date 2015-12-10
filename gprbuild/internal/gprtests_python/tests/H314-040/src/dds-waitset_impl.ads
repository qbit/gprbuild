pragma Ada_05;

with RTI.Obj_Impl;
with DDS.ConditionSeq;
with DDS.Condition;

package DDS.WaitSet_Impl is
   type Ref is new RTI.Obj_Impl.Ref with null record;
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

   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

end DDS.WaitSet_Impl;
