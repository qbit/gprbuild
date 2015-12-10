with Ada.Text_IO; use Ada.Text_IO;
with MLib.Tgt; use MLib.Tgt;

package body MLib.Utl is
   procedure Do_It is
   begin
      Put_Line ("MLib.Utl: Do it");
      Mlib.Tgt.Do_It;
   end Do_It;
end MLib.Utl;
