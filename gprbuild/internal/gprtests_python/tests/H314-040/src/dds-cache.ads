pragma Ada_05;

with DDS.CacheAccess;
with DDS.CacheListener;
with DDS.ObjectHome;

with DDS.Subscriber;
with DDS.Publisher;

package DDS.Cache is

   type Ref is tagged null record;


   function Get_cache_usage
     (Self : in Ref)
     return DDS.CacheUsage;


   function Get_pubsub_state
     (Self : in Ref)
     return DDS.DCPSState;


   function Get_the_publisher
     (Self : in Ref)
      return access DDS.Publisher.Ref'Class;

   function Get_the_subscriber
     (Self : in Ref)
      return access DDS.Subscriber.Ref'Class;

   function Get_updates_enabled
     (Self : in Ref)
     return Boolean;


   procedure register_all_for_pubsub
     (Self : in Ref);

   procedure enable_all_for_pubsub
     (Self : in Ref);

   function register_home
     (Self : in Ref;
      A_Home : access DDS.ObjectHome.Ref'Class)
     return Interfaces.Unsigned_32;

   function find_home_by_name
     (Self : in Ref;
      class_name : in DDS.ClassName)
      return access DDS.ObjectHome.Ref'Class;

   function find_home_by_index
     (Self : in Ref;
      index : in Interfaces.Unsigned_32)
      return access DDS.ObjectHome.Ref'Class;

   procedure attach_listener
     (Self : in Ref;
      listener : in DDS.CacheListener.Ref);

   procedure detach_listener
     (Self : in Ref;
      listener : in DDS.CacheListener.Ref);

   procedure enable_updates
     (Self : in Ref);

   procedure disable_updates
     (Self : in Ref);

   function create_access
     (Self : in Ref;
      purpose : in DDS.CacheUsage)
      return access DDS.CacheAccess.Ref'Class;

   procedure delete_access
     (Self : in Ref;
      IDL_Access : access DDS.CacheAccess.Ref'Class);


   procedure load
     (Self : in Ref);

   procedure lock
     (Self : in Ref;
      to_in_milliseconds : in DDS.TimeOutDuration);

   procedure unlock
     (Self : in Ref);

end DDS.Cache;
