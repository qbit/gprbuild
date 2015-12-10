pragma Ada_05;

with RTI.Obj_Impl;

package DDS.CollectionBase is

   type Abstract_Value_Ref is new RTI.Obj_Impl.Ref with null record;

   function length
     (Self : in Abstract_Value_Ref)
     return Interfaces.Integer_32;

   function is_modified
     (Self : in Abstract_Value_Ref;
      scope : in DDS.ReferenceScope)
     return DDS.Boolean;

   function how_many_added
     (Self : in Abstract_Value_Ref)
     return Interfaces.Integer_32;

   function how_many_removed
     (Self : in Abstract_Value_Ref)
     return Interfaces.Integer_32;

end DDS.CollectionBase;
