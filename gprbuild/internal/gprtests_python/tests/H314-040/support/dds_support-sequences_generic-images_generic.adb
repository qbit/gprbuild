with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body DDS_Support.Sequences_Generic.Images_Generic is

   --------------
   -- Put_Line --
   --------------
   function Image (Item : Sequence; Indent : Standard.String := "") return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
      Data : constant Element_Array := To_Array (Item'Unrestricted_Access);
   begin
      for I in Data'Range loop
         Append (Ret, Image (I, Data (I), Indent));
         Append (Ret, ASCII.Lf);
      end loop;
      return Ret;
   end Image;

end DDS_Support.Sequences_Generic.Images_Generic;
