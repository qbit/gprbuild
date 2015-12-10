with Ada.Text_IO; use Ada.Text_IO;

procedure Make_Sources is
   F : File_Type;
   Main : File_Type;

   function Img (I : Positive) return String is
       Image : constant String := I'Img;
   begin
       return Image (2 .. Image'Last);
   end Img;

begin
   Create (Main, Out_File, "main.adb");
   for J in Positive range 1 .. 1_999 loop
      Create (F, Out_File, "pkg_" & Img (J) & ".ads");
      Put_Line (F, "package Pkg_" & Img (J) & " is");
      Put_Line (F, "   procedure Execute;");
      Put_Line (F, " end;");
      Close (F);
      Create (F, Out_File, "pkg_" & Img (J) & ".adb");
      Put_Line (F, "package body Pkg_" & Img (J) & " is");
      Put_Line (F, "   procedure Execute is begin null; end;");
      Put_Line (F, " end;");
      Close (F);
      Put_Line (Main, "with Pkg_" & Img (J) & ";");
   end loop;
   Put_Line (Main, "procedure Main is");
   Put_Line (Main, "begin");
   for J in Positive range 1 .. 1_999 loop
      Put_Line (Main, "Pkg_" & Img (J) & ".Execute;");
   end loop;
   Put_Line (Main, "end Main;");
   Close (Main);
end Make_Sources;

