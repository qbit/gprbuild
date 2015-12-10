pragma Ada_05;

with DDS.Object;

package DDS.ObjectHome is

   type Ref is new DDS.Object.Ref with null record;

   function Get_Name
     (Self : in Ref)
      return DDS.String;

   function Get_Filter
     (Self : in Ref)
      return DDS.String;

   function Get_Parent
     (Self : in Ref)
      return access DDS.ObjectHome.Ref'Class;


   --     function Get_children
   --       (Self : in Ref)
   --       return DDS.ObjectHomeSeq;

   function Get_Registration_Index
     (Self : in Ref)
      return Interfaces.Unsigned_32;


   --     function Get_refs
   --       (Self : in Ref)
   --       return DDS.ObjectReferenceSeq;

   function Get_Auto_Deref
     (Self : in Ref)
      return DDS.Boolean;

   procedure Set_Filter
     (Self       : in Ref;
      Expression : in DDS.String);

   procedure Set_Auto_Deref
     (Self  : in Ref;
      Value : in DDS.Boolean);

   procedure Deref_All
     (Self : in Ref);

   procedure Underef_All
     (Self : in Ref);


   function Get_Topic_Name
     (Self           : in Ref;
      Attribute_Name : in DDS.String)
      return DDS.String;


   --     function get_all_topic_names
   --       (Self : in Ref)
   --       return DDS.StringSeq;


   --     package Convert_Forward is
   --       new DDS.ObjectHome_Forward.Convert (Ref);
end DDS.ObjectHome;
