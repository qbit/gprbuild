with Ada.Text_IO; use Ada.Text_IO;
with MLib.Utl; use MLib.Utl;

package body MLib is
   procedure Do_It is
   begin
      Put_Line ("MLib: Do it");
      MLib.Utl.Do_It;
   end Do_It;
end Mlib;
