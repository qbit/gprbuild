with Ada.Text_IO;
package body Pack3 is

   procedure Put_Line (S : String) is
   begin
      Ada.Text_IO.Put_Line ("   " & S);
   end;

end;

