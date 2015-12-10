package body Pkg is
   procedure Toto;
   pragma Import (C, Toto);
   procedure Execute is
   begin
      Toto;
   end Execute;
end Pkg;

