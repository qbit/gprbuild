procedure Hello_Ada
is
   procedure Hello;
   pragma Import (C, Hello, "Hello_C");
begin
   Hello;
end Hello_Ada;


