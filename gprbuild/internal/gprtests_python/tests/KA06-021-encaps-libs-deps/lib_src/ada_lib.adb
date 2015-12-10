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

--   task T is
--   end T;

--   task body T is
--   begin
--      null;
--   end;

   --  X : String := Cat ("aaa", "bbb");
   X : Integer := Win32.C;
end Ada_Lib;
