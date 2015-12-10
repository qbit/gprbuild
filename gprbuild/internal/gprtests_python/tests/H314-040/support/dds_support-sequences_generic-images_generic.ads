with Ada.Strings.Unbounded;
generic
   with function Image (Index : in Index_Type; Item : in Element; Indent : Standard.String := "")
     return Ada.Strings.Unbounded.Unbounded_String;
package DDS_Support.Sequences_Generic.Images_Generic is
   function Image (Item : Sequence; Indent : Standard.String := "") return Ada.Strings.Unbounded.Unbounded_String;
end DDS_Support.Sequences_Generic.Images_Generic;
