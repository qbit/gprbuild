pragma Ada_05;
with DDS.DomainParticipant;
with RTI.Obj_Impl;

package DDS.CacheDescription is

   type Value_Ref  is new RTI.Obj_Impl.Ref with null record;

   function get_name
     (Self : not null access Value_Ref)
     return DDS.CacheName;

   procedure set_name
     (Self : not null access Value_Ref;
      To : in DDS.CacheName);

   function get_domain
     (Self : not null access Value_Ref)
      return access DDS.DomainParticipant.Ref'Class;

   procedure set_domain
     (Self : not null access Value_Ref;
      To   : access DDS.DomainParticipant.Ref'Class);

end DDS.CacheDescription;
