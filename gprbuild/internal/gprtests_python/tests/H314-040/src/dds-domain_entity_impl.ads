pragma Ada_05;

with DDS.Entity_Impl;
with DDS.StatusCondition;

package DDS.Domain_Entity_Impl is

   type Ref is new DDS.Entity_Impl.Ref with null record;
   type Ref_Access is access all Ref'Class;

end DDS.Domain_Entity_Impl;
