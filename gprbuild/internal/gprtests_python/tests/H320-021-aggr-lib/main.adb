with API;
with Cst;

procedure Main is
   X : Integer := Cst.Var;
begin
   API.Call (X);
   Cst.Kill;
end Main;
