with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   Put_Line ("hello basic1");
   pragma Assert (False);
exception
   when others =>
      Put_Line ("exception raised");
end Main;
