with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Directories;  use Ada.Directories;
procedure MyDriver is
begin
   Put_Line (Positive'Image (Argument_Count));
   for K in 1 .. Argument_Count loop
      declare
         Arg : constant String := Argument (K);
      begin
         if Arg (Arg'First) = '/' or else
            Arg (Arg'First) = '\'
         then
             Put_Line (Simple_Name (Arg));

         elsif Arg'Length > 9 and then
               Arg (Arg'First .. Arg'First + 8) = "--source="
         then
            Put_Line ("--source=" &
                      Simple_Name (Arg (Arg'First + 9 .. Arg'Last)));

         else 
            Put_Line (Argument (K));
         end if;
      end;
   end loop;
end MyDriver;
