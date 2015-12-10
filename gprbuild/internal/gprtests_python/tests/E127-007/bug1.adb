with P;

procedure Bug1 is
   procedure C_Func;
   pragma Import (C, C_Func);

begin
   C_Func;
end Bug1;
