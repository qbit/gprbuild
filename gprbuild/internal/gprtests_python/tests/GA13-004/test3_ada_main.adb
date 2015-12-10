with Test3_Ada_Model; use Test3_Ada_Model;
with Ada.Tags;        use Ada.Tags;
with Ada.Text_IO;     use Ada.Text_IO;

procedure Test3_Ada_Main is
   pragma Warnings (Off);
   Obj : Object_Type;
begin
   Put_Line (Expanded_Name (Object_Type'Class (Obj)'Tag));
end;
