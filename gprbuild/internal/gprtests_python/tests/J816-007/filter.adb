with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

procedure Filter is

   function Get_File_Names_Case_Sensitive return Integer;
   pragma Import (C, Get_File_Names_Case_Sensitive,
                    "__gnat_get_file_names_case_sensitive");

   function Cur_Dir return String is
      Result : String := Get_Current_Dir;
   begin
      if Get_File_Names_Case_Sensitive = 0 then
        for J in Result'Range loop
            if Result (J) in 'A' .. 'Z' then
               Result (J) := Character'Val (
                          Character'Pos (Result (J)) +
                          Character'Pos ('a')   -
                          Character'Pos ('A'));
            end if;
         end loop;
      end if;
      return Result;
   end Cur_Dir;

   Curdir : constant String := Cur_Dir;

   Map : Character_Mapping := Identity;

   Line : String (1 .. 250);
   Last : Natural;

   Pos  : Natural;

   File : File_Type;
begin
   if Get_File_Names_Case_Sensitive = 0 then
      Map := Lower_Case_Map;
   end if;

   Open (File, In_File, "output.txt");
   while not End_Of_File (File) loop

      Get_Line (File, Line, Last);

      loop
         Pos := Index (Line (1 .. Last), Curdir, Mapping => Map);
         exit when Pos = 0;
         Line (Pos .. Last - Curdir'Length) := Line (Pos + Curdir'Length .. Last);
         Last := Last - Curdir'Length;
      end loop;

      Put_Line (Line (1 .. Last));
   end loop;

end Filter;
