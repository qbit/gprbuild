
with API;

procedure Main is
   V : Integer;

   CV : Integer;
   pragma Import (C, CV, "c_var");

begin
   V := API.Var;
   CV := 1;
end Main;
