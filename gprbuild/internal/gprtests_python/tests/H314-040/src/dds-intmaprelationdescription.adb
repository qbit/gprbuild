pragma Ada_05;
package body DDS.IntMapRelationDescription is

   -------------
   -- get_key --
   -------------

   function get_key
     (Self : access Value_ref)
      return Interfaces.Integer_32
   is
   begin
      return get_key (Self);
   end get_key;

   -------------
   -- set_key --
   -------------

   procedure set_key
     (Self : access Value_ref;
      To : in Interfaces.Integer_32)
   is
   begin
      null;
   end set_key;

end DDS.IntMapRelationDescription;
