with Pkg;
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   function Get_File_Names_Case_Sensitive return Integer;
   pragma Import (C, Get_File_Names_Case_Sensitive,
                    "__gnat_get_file_names_case_sensitive");
   Sensitive : constant Integer := Get_File_Names_Case_Sensitive;
begin
   Put_Line (Sensitive'Img);
   Put_Line (Pkg.Name);
end;

