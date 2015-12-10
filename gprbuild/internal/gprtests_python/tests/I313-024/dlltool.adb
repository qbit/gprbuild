with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

procedure Dlltool is
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
      Ch := Getopt ("-def: -output-lib: -dllname:");

      case Ch is
         when '-' =>
            if Full_Switch = "-def" then
               Input_Name := new String'(Parameter);
            elsif Full_Switch = "-output-lib" then
               Output_Name := new String'(Parameter);
            end if;
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
end Dlltool;
