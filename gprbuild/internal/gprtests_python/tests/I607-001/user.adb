with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Lib;

procedure User
is
begin

   Lib.P;

exception

   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));

end User;
