pragma Ada_05;
package body DDS.IntMapRelation is

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
end DDS.IntMapRelation;
