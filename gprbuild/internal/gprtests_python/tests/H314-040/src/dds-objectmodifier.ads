pragma Ada_05;
-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with DDS.Object;

package DDS.ObjectModifier is

   type Ref is new DDS.Object.Ref with null record;

   Repository_Id : constant Standard.String
     := "IDL:DDS/ObjectModifier:1.0";

end DDS.ObjectModifier;
