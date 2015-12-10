pragma Ada_05;
package body DDS.CacheDescription is

   --------------
   -- get_name --
   --------------

   function get_name
     (Self : not null access Value_Ref)
      return DDS.CacheName
   is
   begin
      return get_name (Self);
   end get_name;

   --------------
   -- set_name --
   --------------

   procedure set_name
     (Self : not null access Value_Ref;
      To : in DDS.CacheName)
   is
   begin
      null;
   end set_name;

   ----------------
   -- get_domain --
   ----------------

   function get_domain
     (Self : not null access Value_Ref)
      return access DDS.DomainParticipant.Ref'Class
   is
   begin
      return get_domain (Self);
   end get_domain;

   ----------------
   -- set_domain --
   ----------------

   procedure set_domain
     (Self : not null access Value_Ref;
      To   : access DDS.DomainParticipant.Ref'Class)
   is
   begin
      null;
   end set_domain;

end DDS.CacheDescription;
