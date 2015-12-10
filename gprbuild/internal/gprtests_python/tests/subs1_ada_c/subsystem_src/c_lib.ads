package C_Lib is
   procedure Do_Something;
   pragma Import (C, Do_Something);
   pragma Assert (False);
end C_Lib;

