pragma Ada_05;

with RTI.Obj_Impl;
package DDS.RelationDescription is

   type Value_Ref is new RTI.Obj_Impl.Ref with null record;


   function Get_Kind
     (Self : access Value_Ref)
      return DDS.RelationKind;

   procedure Set_Kind
     (Self : access Value_Ref;
      To   : in DDS.RelationKind);

   function Get_Name
     (Self : access Value_Ref)
      return DDS.RelationName;

   procedure Set_Name
     (Self : access Value_Ref;
      To   : in DDS.RelationName);

end DDS.RelationDescription;
