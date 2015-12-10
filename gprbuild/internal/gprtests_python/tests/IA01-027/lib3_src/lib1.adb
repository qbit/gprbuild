with lib2;
package body Lib1 is
   procedure P is
   begin
      Lib2.Q;
  end P;

   procedure lib4init;
   pragma Import (C, lib4init, "lib4init");

begin
  Lib4init;
end Lib1;
