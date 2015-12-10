with Ada.Text_IO; use Ada.Text_IO;
with B;

procedure C is

  package Integer_B is new B (Anything_Type => Integer);

  File : File_Type;
  Line : String (1 .. 200);
  Last : Natural;
begin
  Open (File, In_File, "c.ali");
  while not End_Of_File (File) loop
    Get_Line (File, Line, Last);
    if Last > 0 and then Line (1) = 'Z' then
      Put_Line (Line (1 .. Last));
    end if;
  end loop;
  Close (File);
end C;
