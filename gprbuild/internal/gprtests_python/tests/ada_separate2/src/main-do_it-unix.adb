with Ada.Text_IO; use Ada.Text_IO;
with Mlib;

separate (Main)
procedure Do_It is
begin
   Put_Line ("Do UNIX");
   Mlib.Do_It;
end Do_It;
