with Ada.Text_IO; use Ada.Text_IO;
procedure Build is
   function Image (P : Positive) return String is
      Img : constant String := P'Img;
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Image;

   File : File_Type;

begin
   for J in 1 .. 400 loop
      declare
         Img : constant String := Image (J);
      begin
         Create (File, Out_File, "toto" & Img & ".c");
         Put_Line (File, "void toto" & Img & " (void) {}");
         Close (File);
      end;
   end loop;
end Build;

