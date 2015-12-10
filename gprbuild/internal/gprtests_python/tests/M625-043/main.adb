#if toto'defined then
with Ada.Text_IO; use Ada.Text_IO;
#end if;
procedure Main is
begin
#if toto'defined then
   Put_Line ("Toto is defined");
#end if;
   null;
end Main;

