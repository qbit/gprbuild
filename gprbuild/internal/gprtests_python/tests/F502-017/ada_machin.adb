with Ada.Text_IO; use Ada.Text_IO;

package body Ada_Machin is

   package FIO is new Float_IO (C_Float_Type);

   procedure Put_System_Data (System_Data : in C_struct) is

      internal_struct : C_struct;

   begin

      internal_struct.a1 := System_Data.a1;
      internal_struct.a2 := System_Data.a2;

      Put_Line ("(a1 =>" & System_Data.a1'Img & ", a2 =>" &
                 System_Data.a2'Img & ")");

   end Put_System_Data;

   procedure Put_System_Data_Float(System_Data : in C_Float_Type) is
   
      asd : C_Float_Type;
   
   begin
   
      asd := System_Data;
      FIO.Put (asd, 3, 3, 0);
      New_Line;
   
   end Put_System_Data_Float;


end Ada_Machin;
