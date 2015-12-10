with lib2;
package body Lib1 is
   procedure P is
   begin
      Lib2.Q;
  end P;

   procedure lib2init;
   pragma Import (C, lib2init, "lib2init");

begin
  Lib2init;
end Lib1;
