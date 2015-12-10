with Ada.Text_IO; use Ada.Text_IO;

procedure Build_Pkgs is

   Max : constant := 1_000;

   File : File_Type;

   Prj : File_Type;

   Main : File_Type;

   function Image (N : Positive) return String is
      Img : constant String := N'Img;
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Image;

begin
   Create (Main, Out_File, "main.adb");
   Create (Prj, Out_File, "prj.gpr");
   Put_Line (Prj, "project Prj is");
   Put_Line (Prj, "   for Excluded_Source_Files use");
   Put_Line (Prj, "      (""main.adb"", ""build_pkgs.adb"");");
   Put_Line (Prj, "   for Library_Name use ""prj"";");
   Put_Line (Prj, "   for Library_Dir  use ""lib"";");
   Put_Line (Prj, "   for Library_Kind use ""relocatable"";");
   Put_Line (Prj, "   for Library_Interface use (");

   for J in Positive range 1 .. Max loop
      declare
         Num : constant String := Image (J);
      begin
         Create (File, Out_File, "package_number_" & Num & ".ads");
         Put_Line (File, "package Package_Number_" & Num & " is");
         Put_Line (File, "   procedure Execute;");
         Put_Line (File, "end Package_Number_" & Num & ";");
         Close (File);

         Create (File, Out_File, "package_number_" & Num & ".adb");
         Put_Line (File, "with Ada.Text_IO; use Ada.Text_IO;");
         Put_Line (File, "package body Package_Number_" & Num & " is");
         Put_Line (File, "   procedure Execute is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Put_Line (""Package_Number_" & Num & """);");
         Put_Line (File, "   end Execute;");
         Put_Line (File, "end Package_Number_" & Num & ";");
         Close (File);

         Put (Prj, "      ""Package_Number_" & Num & """");
         if J < Max then
            Put_Line (Prj, ",");
         else
            Put_Line (Prj, ");");
         end if;

         Put_Line (Main, "with Package_Number_" & Num & ";");
      end;
  end loop;

  Put_Line (Prj, "end Prj;");
  Close (Prj);

  Put_Line (Main, "procedure Main is");
  Put_Line (Main, "begin");
  Put_Line (Main, "   Package_Number_1.Execute;");
  declare
     Num : constant String := Image (Max);
  begin
     Put_Line (Main, "   Package_Number_" & Num & ".Execute;");
  end;
  Put_Line (Main, "end Main;");
  Close (Main);
end Build_Pkgs;

