package Pkg is
   procedure Toto;
   pragma Export (C, Toto, "toto");
end Pkg;

