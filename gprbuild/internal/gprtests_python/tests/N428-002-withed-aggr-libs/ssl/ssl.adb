package body SSL is
   procedure Whatever;
   pragma Import (C, Whatever, "whatever");

   procedure Execute is
   begin
      Whatever;
   end Execute;
end SSL;
