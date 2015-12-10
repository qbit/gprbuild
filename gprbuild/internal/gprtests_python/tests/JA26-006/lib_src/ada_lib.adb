with Ada.Text_IO; use Ada.Text_IO;
package body Ada_Lib is
   procedure Do_It_In_Ada is
   begin
      Put_Line ("Done in Ada");
   end Do_It_In_Ada;
begin
  Put_Line ("Ada_Lib elaborating");
end Ada_Lib;
