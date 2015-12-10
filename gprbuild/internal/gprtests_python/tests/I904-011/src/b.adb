with Ada.Text_IO; use Ada.Text_IO;
package body B is
   procedure P is
   begin
      Put_Line ("Hello" & I'img);
   end P;
end B;
