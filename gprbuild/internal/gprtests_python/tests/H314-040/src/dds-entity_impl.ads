pragma Ada_05;

with DDS.Entity;
with DDS.StatusCondition;
with DDS.StatusCondition_Impl;
with RTI.Obj_Impl;

package DDS.Entity_Impl is

   type Ref is new RTI.Obj_Impl.Ref and DDS.Entity.Ref with
      record
         StatusCondition : aliased DDS.StatusCondition_Impl.Ref;
      end record;
   type Ref_Access is access all Ref'Class;

   procedure Enable (Self : not null access Ref);

   function Get_StatusCondition (Self : not null access Ref) return
     DDS.StatusCondition.Ref_Access;

   function Get_Status_Changes (Self : not null access Ref) return
     DDS.StatusMask;

   function Get_Instance_Handle (Self : not null access Ref) return
     DDS.InstanceHandle_T;

   procedure Entity_Initialize_I (Self : access Ref;
                                  CEntity : System.Address);

end DDS.Entity_Impl;




