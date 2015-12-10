pragma Ada_05;
package body DDS.StrMapRelationDescription is

   -------------
   -- get_key --
   -------------

   function get_key
     (Self : access Value_ref)
      return DDS.String
   is
   begin
      return get_key (Self);
   end get_key;

   -------------
   -- set_key --
   -------------

   procedure set_key
     (Self : access Value_ref;
      To : in DDS.String)
   is
   begin
      null;
   end set_key;

end DDS.StrMapRelationDescription;
