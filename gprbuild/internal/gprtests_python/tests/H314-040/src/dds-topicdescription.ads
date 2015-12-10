pragma Ada_05;

with DDS.Domain_Entity;
limited with DDS.DomainParticipant;


package DDS.TopicDescription is

   type Ref is interface;
   type Ref_Access is access all Ref'Class;

   function Get_Type_Name
     (Self : not null access Ref)
      return DDS.String is abstract;

   function Get_Name
     (Self : not null access Ref)
      return DDS.String is abstract;

   function Get_Participant
     (Self : not null access Ref)
     return access DDS.DomainParticipant.Ref'Class is abstract;


end DDS.TopicDescription;
