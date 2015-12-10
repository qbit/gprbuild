package body Pck2 is

   procedure C_Routine (V : in Integer);
   pragma Import (C, C_Routine);

   procedure Call is
   begin
      C_Routine (23);
   end Call;

end Pck2;
