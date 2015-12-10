package Pkg1 is
   procedure Execute;
   pragma Export (C, Execute, "execut");
end Pkg1;

