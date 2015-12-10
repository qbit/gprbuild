package body Win32 is

   procedure Toto;
   pragma Import (C, Toto);

   function Cat (S1, S2 : in String) return String is
   begin
      Toto;
      return S1 & S2;
   end Cat;

end Win32;
