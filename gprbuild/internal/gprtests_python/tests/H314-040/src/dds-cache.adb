pragma Ada_05;
package body DDS.Cache is

   ---------------------
   -- Get_cache_usage --
   ---------------------

   function Get_cache_usage
     (Self : in Ref)
      return DDS.CacheUsage
   is
   begin
      return Get_cache_usage (Self);
   end Get_cache_usage;

   ----------------------
   -- Get_pubsub_state --
   ----------------------

   function Get_pubsub_state
     (Self : in Ref)
      return DDS.DCPSState
   is
   begin
      return Get_pubsub_state (Self);
   end Get_pubsub_state;

   -----------------------
   -- Get_the_publisher --
   -----------------------

   function Get_the_publisher
     (Self : in Ref)
      return access DDS.Publisher.Ref'Class
   is
   begin
      return Get_the_publisher (Self);
   end Get_the_publisher;

   ------------------------
   -- Get_the_subscriber --
   ------------------------

   function Get_the_subscriber
     (Self : in Ref)
      return access DDS.Subscriber.Ref'Class
   is
   begin
      return Get_the_subscriber (Self);
   end Get_the_subscriber;

   -------------------------
   -- Get_updates_enabled --
   -------------------------

   function Get_updates_enabled
     (Self : in Ref)
      return Boolean
   is
   begin
      return Get_updates_enabled (Self);
   end Get_updates_enabled;

   -----------------------------
   -- register_all_for_pubsub --
   -----------------------------

   procedure register_all_for_pubsub
     (Self : in Ref)
   is
   begin
      null;
   end register_all_for_pubsub;

   ---------------------------
   -- enable_all_for_pubsub --
   ---------------------------

   procedure enable_all_for_pubsub
     (Self : in Ref)
   is
   begin
      null;
   end enable_all_for_pubsub;

   -------------------
   -- register_home --
   -------------------

   function register_home
     (Self : in Ref;
      A_Home : access DDS.ObjectHome.Ref'Class)
      return Interfaces.Unsigned_32
   is
   begin
      return register_home (Self, A_Home);
   end register_home;

   -----------------------
   -- find_home_by_name --
   -----------------------

   function find_home_by_name
     (Self : in Ref;
      class_name : in DDS.ClassName)
      return access DDS.ObjectHome.Ref'Class
   is
   begin
      return find_home_by_name (Self, class_name);
   end find_home_by_name;

   ------------------------
   -- find_home_by_index --
   ------------------------

   function find_home_by_index
     (Self : in Ref;
      index : in Interfaces.Unsigned_32)
      return access DDS.ObjectHome.Ref'Class
   is
   begin
      return find_home_by_index (Self, index);
   end find_home_by_index;

   ---------------------
   -- attach_listener --
   ---------------------

   procedure attach_listener
     (Self : in Ref;
      listener : in DDS.CacheListener.Ref)
   is
   begin
      null;
   end attach_listener;

   ---------------------
   -- detach_listener --
   ---------------------

   procedure detach_listener
     (Self : in Ref;
      listener : in DDS.CacheListener.Ref)
   is
   begin
      null;
   end detach_listener;

   --------------------
   -- enable_updates --
   --------------------

   procedure enable_updates
     (Self : in Ref)
   is
   begin
      null;
   end enable_updates;

   ---------------------
   -- disable_updates --
   ---------------------

   procedure disable_updates
     (Self : in Ref)
   is
   begin
      null;
   end disable_updates;

   -------------------
   -- create_access --
   -------------------

   function create_access
     (Self : in Ref;
      purpose : in DDS.CacheUsage)
      return access DDS.CacheAccess.Ref'Class
   is
   begin
      return create_access (Self, purpose);
   end create_access;

   -------------------
   -- delete_access --
   -------------------

   procedure delete_access
     (Self : in Ref;
      IDL_Access : access DDS.CacheAccess.Ref'Class)
   is
   begin
      null;
   end delete_access;

   ----------
   -- load --
   ----------

   procedure load
     (Self : in Ref)
   is
   begin
      null;
   end load;

   ----------
   -- lock --
   ----------

   procedure lock
     (Self : in Ref;
      to_in_milliseconds : in DDS.TimeOutDuration)
   is
   begin
      null;
   end lock;

   ------------
   -- unlock --
   ------------

   procedure unlock
     (Self : in Ref)
   is
   begin
      null;
   end unlock;

end DDS.Cache;
