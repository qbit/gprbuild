pragma Ada_05;

with DDS.Topic;
with DDS.Domain_Entity_Impl;
with DDS.TopicListener;
with DDS.TopicDescription;

limited with DDS.DomainParticipant;

with Ada.Unchecked_Deallocation;

package DDS.Topic_Impl is

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.Topic.Ref with null record;
   type Ref_Access is access all Ref'Class;

   --  Re-Implement fron DDS.TopicDescription--

   function Get_Type_Name
     (Self : not null access Ref)
      return DDS.String;

   function Get_Name
     (Self : not null access Ref)
      return DDS.String;

   function Get_Participant
     (Self : not null access Ref)
      return access DDS.DomainParticipant.Ref'Class;

   --
   --
   --

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.TopicQos);

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.TopicQos);

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.TopicListener.Ref_Access;
      Mask       : in DDS.StatusMask);

   function Get_Listener
     (Self : not null access Ref)
      return DDS.TopicListener.Ref_Access;

   procedure Get_Inconsistent_Topic_Status
     (Self     : not null access Ref;
      A_Status : in out DDS.InconsistentTopicStatus;
      Returns  : out DDS.ReturnCode_T);

   procedure Set_Inconsistent_Topic_Status
     (Self     : not null access Ref;
      A_Status : in DDS.InconsistentTopicStatus;
      Returns  : out DDS.ReturnCode_T);

   function Get_TopicDescription (Self : not null access Ref)
                                  return DDS.TopicDescription.Ref_Access;

   procedure Free (This : in out Ref_Access);

   function Get_FacadeI (C_Topic : System.Address)
                         return Ref_Access;

   --   function Get_TopicDescription_ImplI (Self : not null access Ref)
   --                                      return DDS.TopicDescription_Impl.Ref_Access;

   function As_C_TopicDescriptionI (C_Topic : System.Address)
                                    return System.Address;

   function As_C_EntityI  (C_Topic : System.Address)
                           return System.Address;

   function CreateI
     (C_Participant : System.Address;
      Topic_Name    : in DDS.String;
      Type_Name     : in DDS.String;
      Qos           : in DDS.TopicQos;
      A_Listener    : in DDS.TopicListener.Ref_Access;
      Mask          : in DDS.StatusMask)
      return DDS.Topic.Ref_Access;

private
   procedure Free_Impl is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
   procedure Free (This : in out Ref_Access) renames Free_Impl;

   type C_DDS_Topic is record
      As_Entity           : System.Address;
      As_TopicDescription : System.Address;
      Impl                : System.Address;
   end record;
   pragma Convention (C, C_DDS_Topic);
   type C_DDS_Topic_Access is access C_DDS_Topic;

end DDS.Topic_Impl;
