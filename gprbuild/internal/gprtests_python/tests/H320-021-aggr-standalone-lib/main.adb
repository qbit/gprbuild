procedure Main is
   procedure Call (K : Integer);
   pragma Import (Ada, Call, "api__call");
begin
   Call (76);
end Main;
