pragma Ada_05;

with DDS.Condition_Impl;
with Ada.Finalization;
with DDS.Condition;
limited with DDS.GuardCondition_Impl;

package DDS.GuardCondition is

   type Ref is new Ada.Finalization.Controlled and DDS.Condition.Ref with private;
   type Ref_Access is access all Ref'Class;

   procedure Initialize
     (Self : in out Ref);

   procedure Adjust
     (Self : in out Ref);

   procedure Finalize
     (Self : in out Ref);

   procedure Set_Trigger_Value
     (Self : access Ref;
      Value : DDS.Boolean);

   function Get_Trigger_Value
     (Self : access Ref) return DDS.Boolean;

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.Condition_Impl.Ref;

   procedure Free (This : in out Ref_Access);

private

   type Ref is new Ada.Finalization.Controlled and DDS.Condition.Ref with
      record
         Impl_Access : access DDS.GuardCondition_Impl.Ref;
      end record;

   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

end DDS.GuardCondition;
