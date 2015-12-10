pragma Ada_05;
with Interfaces.C.Strings;
package DDS_Support.Strings is
   type String is new Interfaces.C.Strings.char_array_access;


end DDS_Support.Strings;
