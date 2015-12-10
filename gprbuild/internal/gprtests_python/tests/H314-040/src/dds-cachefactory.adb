pragma Ada_05;
package body DDS.CacheFactory is

   ------------------
   -- create_cache --
   ------------------
   function Constructor (Params : not null access Integer) return Ref is
      Ret : Ref;
   begin
      return Ret;
   end Constructor;

   function create_cache
     (Self : not null access Ref;
      cache_usage : in DDS.CacheUsage;
      cache_description : in DDS.CacheDescription.Value_Ref)
      return access DDS.Cache.Ref'Class
   is
   begin
      return create_cache (Self, cache_usage, cache_description);
   end create_cache;

   ------------------------
   -- find_cache_by_name --
   ------------------------

   function find_cache_by_name
     (Self : not null access Ref;
      name : in DDS.CacheName)
      return access DDS.Cache.Ref'Class
   is
   begin
      return find_cache_by_name (Self, name);
   end find_cache_by_name;

   ------------------
   -- delete_cache --
   ------------------

   procedure delete_cache
     (Self : not null access Ref;
      A_Cache : access DDS.Cache.Ref'Class)
   is
   begin
      null;
   end delete_cache;

end DDS.CacheFactory;
