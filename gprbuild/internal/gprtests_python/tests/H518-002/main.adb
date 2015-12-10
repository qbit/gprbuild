with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   null;
#if ADACORE'Defined then
   Put_Line ("ADACORE");
#end if;
#if TOTO'Defined then
   Put_Line ("TOTO");
#end if;
end Main;

