package body Pkg is
   procedure Toto;
   pragma Import (C, Toto, "toto");

   procedure Execute is
   begin
      Toto;
   end Execute;
end Pkg;

