with C_Lib;
With Ada.Text_IO; use Ada.Text_IO;

procedure Ada_Main is

   function F return boolean is
   begin
      Put_Line ("F executed");
      return True;
   end F;

begin
   C_Lib.Do_Something;

   -- F is only called when -gnata is used
   pragma Assert (F);
end Ada_Main;
