
with Ada.Text_IO;

package body Pck10 is

   use Ada;

   procedure Call (V : Integer) is
   begin
      Text_IO.Put_Line ("Ada :" & Integer'Image (V));
   end Call;

end Pck10;
