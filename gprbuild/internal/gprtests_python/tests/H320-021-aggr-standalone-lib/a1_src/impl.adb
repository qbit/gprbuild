with Ada.Text_IO;
package body Impl is
   use Ada;
   procedure Call (V : Integer) is
   begin
      Text_IO.Put_Line ("Call" & Integer'Image (V));
   end Call;
end Impl;
