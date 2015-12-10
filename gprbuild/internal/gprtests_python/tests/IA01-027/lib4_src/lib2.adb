with gnat.io;
package body Lib2 is

   procedure Q is
   begin
      gnat.io.put_line ("lib2.Q from lib4.gpr");
   end Q;
end Lib2;
