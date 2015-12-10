pragma Ada_05;

with DDS.Entity;
with DDS.Condition_Impl;
with DDS.StatusCondition;

package DDS.StatusCondition_Impl is

   type Ref is new DDS.Condition_Impl.Ref and DDS.StatusCondition.Ref with
      record
         Owner : DDS.Entity.Ref_Access;
      end record;

   type Ref_Access is access all Ref'Class;

   function Get_Enabled_Statuses
     (Self : not null access Ref)
      return DDS.StatusMask;

   function Get_Trigger_Value
     (Self : access Ref)
     return DDS.Boolean;

   procedure Set_Enabled_Statuses
     (Self : not null access Ref;
      Mask : in DDS.StatusMask);

   function Get_Entity
     (Self : not null access Ref)
      return access DDS.Entity.Ref'Class;

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.Condition_Impl.Ref;

end DDS.StatusCondition_Impl;
