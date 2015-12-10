
with Ada.Text_IO;

procedure Main is
   use Ada;

   procedure Mine_1;
   pragma Import (C, Mine_1);
   procedure Mine_8;
   pragma Import (C, Mine_8);
   procedure Mine_15;
   pragma Import (C, Mine_15);
   procedure Mine_18;
   pragma Import (C, Mine_18);

begin
   Mine_1;
   Mine_8;
   Mine_15;
   Mine_18;
end Main;
