with Ada.Text_IO; use Ada.Text_IO;
package body Ada_Lib2 is

   procedure Do_It_In_Ada2 is
   begin
      Put_Line ("Done in Ada -2-");
   exception
      when others =>
         Put_Line ("exception raised and handled");
   end Do_It_In_Ada2;

end Ada_Lib2;
