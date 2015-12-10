package Win32 is

   pragma Elaborate_Body;
   pragma Linker_Options ("-lwin32ada");

   C : constant := 12;
   --  function Cat (S1, S2 : in String) return String;

end Win32;
