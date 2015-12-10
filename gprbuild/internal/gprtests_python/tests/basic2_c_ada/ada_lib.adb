with Ada.Text_IO; use Ada.Text_IO;
package body Ada_Lib is

   function F return Integer is
   begin
      return 3;
   end F;

   X : Integer := F;

   procedure Do_It_In_Ada is
   begin
      if X = 3 then
         Put_Line ("Done in Ada");
      else
         Put_Line ("Bad elaboration");
      end if;
   end Do_It_In_Ada;
end Ada_Lib;
