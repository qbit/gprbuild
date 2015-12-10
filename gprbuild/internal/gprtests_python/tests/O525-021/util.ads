with GNAT.OS_Lib; use GNAT.OS_Lib;

package Util is
   Naming_String   : aliased String := "naming";
   List_Of_Packages : aliased String_List :=
                        (1 => Naming_String'Access);
   Packages_To_Check : constant String_List_Access := List_Of_Packages'Access;

end Util;

