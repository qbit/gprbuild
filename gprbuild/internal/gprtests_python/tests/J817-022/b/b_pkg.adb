
with Ada.Text_Io;

package body B_Pkg is

   procedure Do_Something (Name : in String) is
   begin
      Ada.Text_Io.Put_Line ("In B_Pkg.Do_Something Name is " & Name);
   end Do_Something;

end B_Pkg;
