pragma Ada_05;
package body DDS.RefRelation is

   --------------------
   -- is_composition --
   --------------------

   function Is_Composition
     (Self : access Value_Ref)
      return DDS.Boolean
   is
   begin
      return Is_Composition (Self);
   end Is_Composition;

   -----------
   -- reset --
   -----------

   procedure Reset
     (Self : access Value_Ref)
   is
   begin
      null;
   end Reset;

   -----------------
   -- is_modified --
   -----------------

   function Is_Modified
     (Self  : access Value_Ref;
      Scope : in DDS.ReferenceScope)
      return DDS.Boolean
   is
   begin
      return Is_Modified (Self, Scope);
   end Is_Modified;

end DDS.RefRelation;
