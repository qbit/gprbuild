procedure Main is
   procedure Src1;
   pragma Import (C, Src1);
   procedure Src2;
   pragma Import (C, Src2);
begin
   Src1;
   Src2;
end Main;

