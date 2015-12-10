with Ada.Text_IO; use Ada.Text_IO;
package body Pck2 is
   procedure Call (Name : String := Pck1.Val) is
   begin
      Put_Line (Name);
   end Call;
end Pck2;
