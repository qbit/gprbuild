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

package DDS.CacheListener is

   type Ref is new DDS.Object.Ref with null record;

   procedure begin_updates
     (Self : in Ref;
      update_round : in Interfaces.Integer_32);

   procedure end_updates
     (Self : in Ref;
      update_round : in Interfaces.Integer_32);

   function Constructor (Params : not null access Integer) return Ref;
end DDS.CacheListener;
