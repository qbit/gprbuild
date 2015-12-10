with Ada.Text_IO;
with Interfaces.C;

package body AdaTestAda is
   This : Object;
   
   procedure Configure is
   begin
      This.m_flag := False;
   end Configure;

   procedure Update is
   begin 
      This.m_flag := False;
   end Update;

end AdaTestAda;
