with Ada.Text_IO;
package body API is
   use Ada;
   procedure Call (V : Integer) is
   begin
      Text_IO.Put_Line ("Call" & Integer'Image (V));
   end Call;
end API;
