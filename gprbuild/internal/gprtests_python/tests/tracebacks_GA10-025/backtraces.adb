with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

procedure Backtraces is
   procedure Do_It_1 is
      procedure Do_It_2 is
         procedure Do_It_3 is
         begin
            raise Program_Error;
         end Do_It_3;
      begin
         Do_It_3;
      end Do_It_2;
   begin
      Do_It_2;
   end Do_It_1;
begin
   Do_It_1;
exception
   when PE : Program_Error =>
      Put_Line (Symbolic_Traceback (PE));
end Backtraces;
