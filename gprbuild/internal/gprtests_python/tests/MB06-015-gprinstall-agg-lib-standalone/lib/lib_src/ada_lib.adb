with Ada.Text_IO; use Ada.Text_IO;
with Pck1;
package body Ada_Lib is

   X : String := "aaa" & "bbb";

   procedure Do_It_In_Ada is
   begin
      Put_Line ("Done in Ada:" & X & "+" & Pck1.Name);
   end Do_It_In_Ada;

end Ada_Lib;
