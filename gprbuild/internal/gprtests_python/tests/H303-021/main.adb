with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   procedure Toto;
   pragma Import (C, Toto, "toto");
begin
   Put_Line ("main.adb");
   Toto;
end Main;

