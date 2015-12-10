with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO; use Ada.Text_IO;

package body P is

   Base : constant String := "foreign thread";

   procedure Ada_Routine is
      Img : constant String := Image (Current_Task);
   begin
      if Img'Length < Base'Length
        or else Img (1 .. Base'Length) /= Base
      then
         Put_Line ("failed");
      else
         Put_Line ("succeeded");
      end if;
   end Ada_Routine;

end P;
