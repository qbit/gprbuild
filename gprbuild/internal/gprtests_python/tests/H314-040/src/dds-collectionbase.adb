pragma Ada_05;
package body DDS.CollectionBase is

   ------------
   -- length --
   ------------

   function length
     (Self : in Abstract_Value_Ref)
      return Interfaces.Integer_32
   is
   begin
      return length (Self);
   end length;

   -----------------
   -- is_modified --
   -----------------

   function is_modified
     (Self : in Abstract_Value_Ref;
      scope : in DDS.ReferenceScope)
      return DDS.Boolean
   is
   begin
      return is_modified (Self, scope);
   end is_modified;

   --------------------
   -- how_many_added --
   --------------------

   function how_many_added
     (Self : in Abstract_Value_Ref)
      return Interfaces.Integer_32
   is
   begin
      return how_many_added (Self);
   end how_many_added;

   ----------------------
   -- how_many_removed --
   ----------------------

   function how_many_removed
     (Self : in Abstract_Value_Ref)
      return Interfaces.Integer_32
   is
   begin
      return how_many_removed (Self);
   end how_many_removed;

end DDS.CollectionBase;
