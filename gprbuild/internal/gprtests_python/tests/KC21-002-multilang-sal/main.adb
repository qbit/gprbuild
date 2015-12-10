with Pkg;
procedure Main is
   procedure Titi;
   pragma Import (C, Titi);
begin
   Pkg.Execute;
   Titi;
end Main;

