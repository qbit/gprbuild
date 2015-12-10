package Pkg is
   procedure Execute;
end Pkg;

with Ada.Text_IO; use Ada.Text_IO;
package body Pkg is
   procedure Execute is
   begin
      Put_Line ("Pkg.Execute");
   end Execute;
end Pkg;

with Pkg;
with Pkg2;
procedure Main is
begin
   Pkg.Execute;
end Main;

with Pkg;
with Pkg2;
procedure Main2 is
begin
   Pkg.Execute;
   Pkg2.Execute;
end Main2;


