with Ada.Text_IO; use Ada.Text_IO;

procedure Sort_Output is
   File : File_Type;
   Line : String (1 .. 1_000);
   Last : Natural;

   type String_Access is access String;
   type Strings is array (Positive range <>) of String_Access;

   Lines : Strings (1 .. 100);
   Last_Line : Natural := 0;

   Cur : Positive;

begin
   Open (File, In_File, "output.txt");

   while not End_Of_File (File) loop
      Get_Line (File, Line, Last);

      Last_Line := Last_Line + 1;
      Lines (Last_Line) := new String'(Line (1 .. Last));

      Cur := Last_Line;
      while Cur > 1 loop
         exit when Lines (Cur - 1).all <= Lines (Cur).all;

         Lines (Last_Line + 1) := Lines (Cur - 1);
         Lines (Cur - 1) := Lines (Cur);
         Lines (Cur) := Lines (Last_Line + 1);

         Cur := Cur - 1;
      end loop;
   end loop;

   for J in 1 .. Last_Line loop
      Put_Line (Lines (J).all);
   end loop;
end Sort_Output;

            
