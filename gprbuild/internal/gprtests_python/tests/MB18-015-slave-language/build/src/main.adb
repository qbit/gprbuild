
with Ada.Text_IO;

with Pck1;
with Pck2;
with Pck3;
with Pck4;
with Pck5;
with Pck6;
with Pck7;
with Pck8;
with Pck9;
with Pck10;

procedure Main is
   use Ada;
   procedure M_Code;
   pragma Import (C, M_Code);
   procedure M_Code4;
   pragma Import (C, M_Code4);
   procedure M_Code9;
   pragma Import (C, M_Code9);
begin
   Text_IO.Put_Line ("Hello!");
   Pck10.Call (8);
   M_Code;
   M_Code4;
   M_Code9;
end Main;
