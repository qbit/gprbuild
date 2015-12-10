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

package DDS.Selection is

   type Ref is new DDS.Object.Ref with null record;

   function Get_auto_refresh
     (Self : in Ref)
     return DDS.Boolean;

   function Get_concerns_contained
     (Self : in Ref)
     return DDS.Boolean;

   procedure refresh
     (Self : in Ref);

end DDS.Selection;
