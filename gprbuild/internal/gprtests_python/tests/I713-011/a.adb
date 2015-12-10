with B;
with C;
procedure A is
   function Toto return Integer;
   pragma Import (C, Toto, "toto");
   D : Integer;
begin
   D := Toto;
   B.Foo;
   C.Bar;
end A;
