
pragma Ada_05;

with DDS.MultiTopic;
with DDS.Domain_Entity_Impl;
limited with DDS.DomainParticipant;

with DDS.StatusCondition;

package DDS.MultiTopic_Impl is

   type Ref is new DDS.Domain_Entity_Impl.Ref and DDS.MultiTopic.Ref with null record;
   type Ref_Access is access all Ref'Class;

   --  Re-Implement From DDS.Entity

   procedure Enable (Self : not null access Ref);

   function Get_StatusCondition (Self : not null access Ref) return
     DDS.StatusCondition.Ref_Access;

   function Get_Status_Changes (Self : not null access Ref) return
     DDS.StatusMask;

   function Get_Instance_Handle (Self : not null access Ref) return
     DDS.InstanceHandle_T;

   --  Re-Implement fron DDS.TopicDescription
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
   function get_subscription_expression
     (Self : not null access Ref)
     return DDS.String;

--     function get_expression_parameters
--       (Self : not null access Ref)
--       return DDS.StringSeq;
--
--
--     function set_expression_parameters
--       (Self : not null access Ref;
--        expression_parameters : in DDS.StringSeq)
--       return DDS.ReturnCode_t;


end DDS.MultiTopic_Impl;
