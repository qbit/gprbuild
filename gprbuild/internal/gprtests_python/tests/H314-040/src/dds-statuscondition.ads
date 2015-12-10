pragma Ada_05;

with DDS.Condition;
limited with DDS.Entity;

package DDS.StatusCondition is

   type Ref is interface and DDS.Condition.Ref;
   type Ref_Access is access all Ref'Class;

   function Get_Enabled_Statuses
     (Self : not null access Ref)
      return DDS.StatusMask is abstract;

   procedure Set_Enabled_Statuses
     (Self : not null access Ref;
      Mask : in DDS.StatusMask) is abstract;

   function Get_Entity
     (Self : not null access Ref)
      return access DDS.Entity.Ref'Class is abstract;

end DDS.StatusCondition;

