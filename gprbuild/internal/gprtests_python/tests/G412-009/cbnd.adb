with GNAT.OS_Lib; use GNAT.OS_Lib;
procedure Cbnd is
   GCC : constant String_Access := Locate_Exec_On_Path ("gcc");
   GCC_Args : String_List :=
     (new String'("-c"),
      new String'("-x"),
      new String'("c"),
      new String'("toto.cc"),
      new String'("-o"),
      new String'("toto.o"));
   CP  : constant String_Access := Locate_Exec_On_Path ("cp");
   CP_Args : String_List :=
     (new String'("c__main.bexch.saved"),
      new String'("c__main.bexch"));
   Dummy : Boolean;
begin
   Spawn (GCC.all, GCC_Args, Dummy);
   Spawn (CP.all, CP_Args, Dummy);
end Cbnd;

