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

with DDS.Cache;
with DDS.CacheDescription;
with DDS.Object;

package DDS.CacheFactory is

   type Ref is new DDS.Object.Ref with null record;

   function create_cache
     (Self : not null access Ref;
      cache_usage : in DDS.CacheUsage;
      cache_description : in DDS.CacheDescription.Value_Ref)
      return access DDS.Cache.Ref'Class;

   function find_cache_by_name
     (Self : not null access Ref;
      name : in DDS.CacheName)
      return access DDS.Cache.Ref'Class;

   procedure delete_cache
     (Self : not null access Ref;
      A_Cache : access DDS.Cache.Ref'Class);

   function Constructor (Params : not null access Integer) return Ref;
end DDS.CacheFactory;
