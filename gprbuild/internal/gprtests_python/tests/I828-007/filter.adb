with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
procedure Filter is
   Arg1 : constant String := Argument (1);
   Arg2 : constant String := Argument (2);
   Cur : Positive;
   File : File_Type;
begin
   Open (File, Append_File, "output.txt");

   if Arg1'Length <= 2 or else Arg1 (Arg1'First .. Arg1'First + 1) /= "-I" then
      Put (Arg1);
         
   else
      Put (File, "-I");
   end if;

   Put (File, " ");

   Cur := Arg2'Last;
   while Cur > Arg2'First and then
         Arg2 (Cur) /= '/' and then
         Arg2 (Cur) /= '\'
   loop
      Cur := Cur - 1;
   end loop;

   Put_Line (File, Arg2 (Cur + 1 .. Arg2'Last));
   Close (File);
end Filter;
