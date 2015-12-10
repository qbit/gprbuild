with Ada.Text_IO; use Ada.Text_IO;
with SSL;
package body Src is
   procedure Execute is
   begin
      SSL.Execute;
   end Execute;
end Src;

