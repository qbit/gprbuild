procedure main is
   procedure C_Proc;
   pragma Import (C, C_Proc, "c_proc");
begin
   C_Proc;
end main;
