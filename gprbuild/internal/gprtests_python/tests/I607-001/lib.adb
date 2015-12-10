with Ada.Text_IO; use Ada.Text_IO;
with Inner;

package body Lib is

   procedure P
   is
   begin
      Put_Line ("lib.p called");
      Inner.Q;
   end P;

begin
   Put_Line ("lib elaborated");
end Lib;
