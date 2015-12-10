package Ext_API is
   C_Var : Integer := 1;
   pragma Export (C, C_Var, "c_var");
end Ext_API;
