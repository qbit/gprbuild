pragma Ada_05;
package body DDS.ListRelationDescription is

   ---------------
   -- get_index --
   ---------------

   function get_index
     (Self : access Value_ref)
      return Interfaces.Integer_32
   is
   begin
      return get_index (Self);
   end get_index;

   ---------------
   -- set_index --
   ---------------

   procedure set_index
     (Self : access Value_ref;
      To : in Interfaces.Integer_32)
   is
   begin
      null;
   end set_index;

end DDS.ListRelationDescription;
