with GNAT.IO; use GNAT.IO;
package body My_ctrl is


   procedure Initialize ( X : in out T) is
   begin
      X.S := Init;
   end Initialize;
   procedure Finalize ( X : in out T) is
   begin
      X.S := Final;
   end Finalize;

   -----------------
   -- Show_Status --
   -----------------

   procedure Show_Status (X : String; O : T) is
   begin
      Put (X);
      Put (" : ");
      Put_Line (O.S'Img);
   end Show_Status;

end My_ctrl;
