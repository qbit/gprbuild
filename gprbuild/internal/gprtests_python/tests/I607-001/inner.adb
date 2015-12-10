with Ada.Text_IO; use Ada.Text_IO;

package body Inner is

--   task T;

--   task body T
--   is
--   begin
--      Put_Line ("inner.t starting");
--      delay 1.0;
--   end T;

   procedure Q
   is
   begin
      Put_Line ("inner.q called");
   end Q;

begin
   Put_Line ("inner elaborated");
end Inner;
