pragma Ada_05;
package body DDS.RelationDescription is

   --------------
   -- get_kind --
   --------------

   function Get_Kind
     (Self : access Value_Ref)
      return DDS.RelationKind
   is
   begin
      return Get_Kind (Self);
   end Get_Kind;

   --------------
   -- set_kind --
   --------------

   procedure Set_Kind
     (Self : access Value_Ref;
      To   : in DDS.RelationKind)
   is
   begin
      null;
   end Set_Kind;

   --------------
   -- get_name --
   --------------

   function Get_Name
     (Self : access Value_Ref)
      return DDS.RelationName
   is
   begin
      return Get_Name (Self);
   end Get_Name;

   --------------
   -- set_name --
   --------------

   procedure Set_Name
     (Self : access Value_Ref;
      To   : in DDS.RelationName)
   is
   begin
      null;
   end Set_Name;

end DDS.RelationDescription;
