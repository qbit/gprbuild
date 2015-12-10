pragma Ada_05;
package body DDS.Selection is

   ----------------------
   -- Get_auto_refresh --
   ----------------------

   function Get_Auto_Refresh
     (Self : in Ref)
      return DDS.Boolean
   is
   begin
      return Get_Auto_Refresh (Self);
   end Get_Auto_Refresh;

   ----------------------------
   -- Get_concerns_contained --
   ----------------------------

   function Get_Concerns_Contained
     (Self : in Ref)
      return DDS.Boolean
   is
   begin
      return Get_Concerns_Contained (Self);
   end Get_Concerns_Contained;

   -------------
   -- refresh --
   -------------

   procedure Refresh
     (Self : in Ref)
   is
   begin
      null;
   end Refresh;

end DDS.Selection;
