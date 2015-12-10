procedure Main is

   procedure Toto;
   pragma Import (C, Toto);

begin
   Toto;
end Main;

