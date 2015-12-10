pragma Ada_05;

with RTI.Obj_Impl;

package DDS.RefRelation is

   type Value_Ref is new RTI.Obj_Impl.Ref with null record;

   function Is_Composition
     (Self : access Value_Ref)
      return DDS.Boolean;

   procedure Reset
     (Self : access Value_Ref);


   function Is_Modified
     (Self  : access Value_Ref;
      Scope : in DDS.ReferenceScope)
      return DDS.Boolean;


end DDS.RefRelation;
