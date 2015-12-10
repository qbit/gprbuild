with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
use Ada.Directories;
with GNAT.Command_Line;

procedure Make_Sources is
   Rootprj : File_Type;
   function Img (I : Positive) return String is
      Image : constant String := I'Img;
   begin
      return Image (2 .. Image'Last);
   end Img;

   CPP_Count      : aliased Integer := 0;
   Ada_Count      : aliased Integer := 0;
   Project_Count  : aliased Integer := 0;

   procedure Make_Proj (No : Integer) is
      Name : constant String := "proj_" & Img (No);
      procedure Make_CPP is
         F : File_Type;
      begin
         for J in 1 .. CPP_Count loop
            declare
               CPP_Name : constant String := "cpp" & Img (J);
            begin
               Create (F, Out_File, Compose (Name, CPP_Name & ".cpp"));
               Put_Line (F, "void " & CPP_Name & "_" & Img (No) & "(void)");
               Put_Line (F, "{");
               Put_Line (F, "}");
               Close (F);
            end;
         end loop;
      end;

      procedure Make_Ada is
         F : File_Type;
      begin
         for J in 1 .. Ada_Count loop
            declare
               Ada_Name : constant String := "adaunit_" & Img (No) & "_" & Img (J);
            begin
               Create (F, Out_File, Compose (Name, Ada_Name & ".ads"));
               Put_Line (F, "package " & Ada_Name & " is ");
               Put_Line (F, "task type foo is");
               Put_Line (F, "end foo;");
               Put_Line (F, "end " & Ada_Name & ";");
               Close (F);
               Create (F, Out_File, Compose (Name, Ada_Name & ".adb"));
               Put_Line (F, "package body " & Ada_Name & " is ");
               Put_Line (F, "task body foo is");
               Put_Line (F, "begin");
               Put_Line (F, "null;");
               Put_Line (F, "end foo;");
               Put_Line (F, "end " & Ada_Name & ";");
               Close (F);
            end;
         end loop;
      end;
      F : File_Type;

   begin

      Ada.Directories.Create_Path ("proj_" & Img (No));
      Create (F, Out_File, Compose (Name, Name & ".gpr"));
      Put_Line (Rootprj, "with """ & Compose (Name, Name & ".gpr") & """;");
      Put_Line (F, "project " & Name & " is");
      Put_Line (F, "   for Languages use (""Ada"");");
      Put_Line (F, "   for Library_Name use Project'Name;");
      Put_Line (F, "   for Library_Dir  use ""lib"";");
      Put_Line (F, "   for Library_Kind use ""relocatable"";");
      Put_Line (F, "   for library_Options use (""-lpthread"");");
      Put_Line (F, "end " & Name & ";");
      Close (F);
      Create (F, Out_File, "root.ads");
      Put_Line (F, "package root is ");
      Put_Line (F, "end root;");
      Close (F);

      Create (F, Out_File, "cpproot.cpp");
      Put_Line (F, "void cppRooy(){};");
      Close (F);

      Make_CPP;
      Make_Ada;

   end;
   Conf : GNAT.Command_Line.Command_Line_Configuration;
   use GNAT.Command_Line;
begin

   Define_Switch (Conf, CPP_Count'Access, Switch => "-C=", Help => "CPP_Count");
   Define_Switch (Conf, Ada_Count'Access, Switch => "-A=", Help => "Ada_Count");
   Define_Switch (Conf, Project_Count'Access, Switch => "-P=", Help => "Projrect_Count");
   Getopt (Conf);


   Put_Line (Project_Count'Img  & Ada_Count'Img & CPP_Count'Img);
   Create (Rootprj, Out_File, "root.gpr");

   for I in 1 .. Project_Count loop
      Make_Proj (I);
   end loop;
   Put_Line (Rootprj, "project root is");
   Put_Line (Rootprj, "   for Languages use (""Ada"", ""C++"");");
   Put_Line (Rootprj, "   for Library_Name use Project'Name;");
   Put_Line (Rootprj, "   for Library_Dir  use ""lib"";");
   Put_Line (Rootprj, "   for Library_Kind use ""relocatable"";");
   Put_Line (Rootprj, "   for library_Options use (""-lpthread"");");
   Put_Line (Rootprj, "end  root;");
   Close (Rootprj);
end Make_Sources;
