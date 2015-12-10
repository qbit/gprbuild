with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Toto is
   File : File_Type;

   type String_Access is access String;

   Dep : String_Access;
   Src : String_Access;

begin
   for J in 1 .. Argument_Count loop
      declare
         Arg : constant String := Argument (J);
      begin
         if Arg'Length > 2 and then
            Arg (Arg'First .. Arg'First + 1) = "-d"
         then
            Dep := new String'(Arg (Arg'First + 2 .. Arg'Last));

         else
            Src := new String'(Arg);
         end if;
      end;
   end loop;

   if Dep /= null and then Src /= null then
      Create (File, Out_File, Dep.all);
      Put_Line (File, Dep.all & ": " & Src.all);
      Close (File);
   end if;

end Toto;

