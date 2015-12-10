procedure Main is
   procedure Titi;
   pragma Import (C, Titi, "titi");
begin
   Titi;
end Main;

