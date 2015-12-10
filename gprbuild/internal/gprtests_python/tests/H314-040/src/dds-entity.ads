pragma Ada_05;

with DDS.StatusCondition;

package DDS.Entity is

   type Ref is limited interface;
   type Ref_Access is access all Ref'Class;

   procedure Enable (Self : not null access Ref) is abstract;

   function Get_StatusCondition (Self : not null access Ref) return
     DDS.StatusCondition.Ref_Access is abstract;

   function Get_Status_Changes (Self : not null access Ref) return
     DDS.StatusMask is abstract;

   function Get_Instance_Handle (Self : not null access Ref) return
     DDS.InstanceHandle_T is abstract;

end DDS.Entity;
