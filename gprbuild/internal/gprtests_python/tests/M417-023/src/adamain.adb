with Pkg;
with Pkg2;
package body AdaMain is
   procedure Execute is
   begin
      Pkg.Execute;
      Pkg2.Execute;
   end Execute;
end AdaMain;

