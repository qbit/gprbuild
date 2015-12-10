package Pck is
   procedure Call;
#if TARGET = "Win32"
   pragma Import (C, Call, "call_1");
#else
   pragma Import (C, Call, "call_2");
#end if;
end Pck;
