with Ada.Text_IO; use Ada.Text_IO;
-- with Ada_lib2; use ada_lib2;
with win32; use Win32;
package body Ada_Lib is

   procedure Do_It_In_Ada is
   begin
      Put_Line ("Done in Ada");
   exception
      when others =>
         Put_Line ("exception raised and handled");
   end Do_It_In_Ada;

   X : String := Cat ("aaa", "bbb");

end Ada_Lib;
