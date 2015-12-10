with lib;
with ctrl_obj;
with My_ctrl; use my_ctrl;
procedure Main is
   procedure Init;
   pragma Import (C, Init, "mylibinit");
   procedure Final;
   pragma Import (C, Final, "mylibfinal");

begin
   Show_Status ("before init  <global>", ctrl_obj.x);
   Init;
   Show_Status ("before final <global>", ctrl_obj.x);
   Show_Status ("before final <lib>   ", lib.x);
   Final;
   Show_Status ("after final <global> ", ctrl_obj.x);
   Show_Status ("after final <lib>    ", lib.x);
end Main;
