with Pck9;
package Pck10 is
   V : Integer := Pck9.V + 1;
   procedure Call (V : Integer);
   pragma Import (C, Call);
end;
