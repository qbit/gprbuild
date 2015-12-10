with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

procedure Windres is
   type String_Access is access String;

   Input_Name : String_Access;
   Input      : File_Type;

   Output_Name : String_Access;
   Output      : File_Type;

   Ch : Character;

   Line : String (1 .. 1_000);
   Last : Natural;

begin
   loop
      Ch := Getopt ("i: o:");

      case Ch is
         when 'i' => Input_Name := new String'(Parameter);
         when 'o' => Output_Name := new String'(Parameter);
         when others => exit;
      end case;
   end loop;

   if Input_Name /= null and then Output_Name /= null then
      Open (Input, In_File, Input_Name.all);
      Create (Output, Out_File, Output_Name.all);

      while not End_Of_File (Input) loop
         Get_Line (Input, Line, Last);
         Put_Line (Output, Line (1 .. Last));
      end loop;

      Close (Input);
      Close (Output);
   end if;
end Windres;
