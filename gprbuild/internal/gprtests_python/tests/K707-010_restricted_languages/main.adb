with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   procedure Toto;
   pragma Import (C, Toto);
begin
   Toto;
   Put_Line ("Main");
end Main;
