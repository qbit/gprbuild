with Pck;
with DOM;
procedure Main is
   S : Dom.Dom_String := "";
   procedure Whatever;
   pragma Import (C, Whatever);
begin
   Pck.Call;
   Whatever;
end Main;
