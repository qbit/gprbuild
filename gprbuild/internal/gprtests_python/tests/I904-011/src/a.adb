with B;
with Ada.Text_IO; use Ada.Text_IO;
package body A is
   procedure P (X : String) is
   begin
      B.P;
      Put_Line (X);
   end;
end A;
