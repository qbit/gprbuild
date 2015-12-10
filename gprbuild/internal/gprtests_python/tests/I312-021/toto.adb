with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure Toto is
begin
   for J in 1 .. Argument_Count loop
      Put (Argument (J));

      if J /= Argument_Count then
         Put (' ');
      end if;
   end loop;

   New_Line;
end Toto;
