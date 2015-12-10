package Pck10 is
   procedure Call (V : Integer);
   pragma Import (C, Call);
end;
