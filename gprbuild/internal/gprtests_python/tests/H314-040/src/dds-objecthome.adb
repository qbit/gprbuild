pragma Ada_05;
package body DDS.ObjectHome is

   --------------
   -- Get_name --
   --------------

   function Get_Name
     (Self : in Ref)
      return DDS.String
   is
   begin
      return Get_Name (Self);
   end Get_Name;

   ----------------
   -- Get_filter --
   ----------------

   function Get_Filter
     (Self : in Ref)
      return DDS.String
   is
   begin
      return Get_Filter (Self);
   end Get_Filter;

   ----------------
   -- Get_parent --
   ----------------

   function Get_Parent
     (Self : in Ref)
      return access DDS.ObjectHome.Ref'Class
   is
   begin
      return Get_Parent (Self);
   end Get_Parent;

   ----------------------------
   -- Get_registration_index --
   ----------------------------

   function Get_Registration_Index
     (Self : in Ref)
      return Interfaces.Unsigned_32
   is
   begin
      return Get_Registration_Index (Self);
   end Get_Registration_Index;

   --------------------
   -- Get_auto_deref --
   --------------------

   function Get_Auto_Deref
     (Self : in Ref)
      return DDS.Boolean
   is
   begin
      return Get_Auto_Deref (Self);
   end Get_Auto_Deref;

   ----------------
   -- set_filter --
   ----------------

   procedure Set_Filter
     (Self       : in Ref;
      Expression : in DDS.String)
   is
   begin
      null;
   end Set_Filter;

   --------------------
   -- set_auto_deref --
   --------------------

   procedure Set_Auto_Deref
     (Self  : in Ref;
      Value : in DDS.Boolean)
   is
   begin
      null;
   end Set_Auto_Deref;

   ---------------
   -- deref_all --
   ---------------

   procedure Deref_All
     (Self : in Ref)
   is
   begin
      null;
   end Deref_All;

   -----------------
   -- underef_all --
   -----------------

   procedure Underef_All
     (Self : in Ref)
   is
   begin
      null;
   end Underef_All;

   --------------------
   -- get_topic_name --
   --------------------

   function Get_Topic_Name
     (Self           : in Ref;
      Attribute_Name : in DDS.String)
      return DDS.String
   is
   begin
      return Get_Topic_Name (Self, Attribute_Name);
   end Get_Topic_Name;

   -------------------------
   -- get_all_topic_names --
   -------------------------

   --     function get_all_topic_names
   --       (Self : in Ref)
   --        return DDS.StringSeq
   --     is
   --     begin
   --        return get_all_topic_names (Self);
   --     end get_all_topic_names;

end DDS.ObjectHome;
