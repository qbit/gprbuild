with Ada.Text_IO; use Ada.Text_IO;
procedure Main2 is
   procedure Toto;
   pragma Import (C, Toto, "toto");
begin
   Put_Line ("main2.adb");
   Toto;
end Main2;

