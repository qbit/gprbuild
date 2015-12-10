with Ada.Text_IO; use Ada.Text_IO;
with Mlib;

separate (Main)
procedure Do_It is
begin
   Put_Line ("Do VMS");
   Mlib.Do_It;
end Do_It;
