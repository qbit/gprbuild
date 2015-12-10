pragma Ada_05;

with DDS.Domain_Entity;
with DDS.TopicListener;
with DDS.TopicDescription;

package DDS.Topic is

   type Ref is interface and DDS.Domain_Entity.Ref and DDS.TopicDescription.Ref;
   type Ref_Access is access all Ref'Class;

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.TopicQos) is abstract;

   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.TopicQos) is abstract;

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.TopicListener.Ref_Access;
      Mask       : in DDS.StatusMask) is abstract;

   function Get_Listener
     (Self : not null access Ref)
      return DDS.TopicListener.Ref_Access is abstract;

   procedure Get_Inconsistent_Topic_Status
     (Self     : not null access Ref;
      A_Status : in out DDS.InconsistentTopicStatus;
      Returns  : out DDS.ReturnCode_T) is abstract;

   procedure Set_Inconsistent_Topic_Status
     (Self     : not null access Ref;
      A_Status : in DDS.InconsistentTopicStatus;
      Returns  : out DDS.ReturnCode_T) is abstract;

   function Get_TopicDescription (Self : not null access Ref) return
     DDS.TopicDescription.Ref_Access is abstract;

end DDS.Topic;
