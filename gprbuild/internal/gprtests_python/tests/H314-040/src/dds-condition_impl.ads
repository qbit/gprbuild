pragma Ada_05;

with RTI.Obj_Impl;
with DDS.Condition;

package DDS.Condition_Impl is

   type Ref is abstract new RTI.Obj_Impl.Ref and DDS.Condition.Ref with null record;
   type Ref_Access is access all Ref'Class;

end DDS.Condition_Impl;
