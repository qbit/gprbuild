pragma Ada_05;

with RTI.Obj_Impl;
with DDS.DomainParticipant;
with DDS.DomainParticipantListener;
package DDS.DomainParticipantFactory is

   type Ref is new RTI.Obj_Impl.Ref with private;
   type Ref_Access is access all Ref'Class;

   PARTICIPANT_QOS_DEFAULT : DDS.DomainParticipantQos;

   function Get_Instance return Ref_Access;

   function Create_Participant
     (Self       : not null access Ref;
      Domain_Id  : in DDS.DomainId_T;
      Qos        : in DDS.DomainParticipantQos;
      A_Listener : in DDS.DomainParticipantListener.Ref_Access;
      Mask       : in DDS.StatusMask)
      return DDS.DomainParticipant.Ref_Access;

   procedure Delete_Participant
     (Self          : not null access Ref;
      A_Participant : in out DDS.DomainParticipant.Ref_Access);


   function Lookup_Participant
     (Self      : not null access Ref;
      Domain_Id : in DDS.DomainId_T)
      return DDS.DomainParticipant.Ref_Access;


   procedure Set_Default_Participant_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DomainParticipantQos);


   procedure Get_Default_Participant_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.DomainParticipantQos);


   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DomainParticipantFactoryQos);
   --  Sets the value for a participant factory QoS.
   --  The DDS_DomainParticipantFactoryQos::entity_factory can be changed. The other policies are immutable.
   --  Note that despite having QoS, the DDS_DomainParticipantFactory is not an DDS_Entity.
   --  Parameters:
   --     self <<in>> Cannot be NULL.
   --     qos  <<in>> Set of policies to be applied to DDS_DomainParticipantFactory. Policies must be consistent.
   --                 Immutable Policies Can Only Be Changed Before Calling Any Other
   --                 Data Distribution Service Functions Except for DDS_DomainParticipantFactory_Get_Qos.
   --  Raises:
   --     One of the Standard Return Codes, DDS_RETCODE_IMMUTABLE_POLICY if immutable policy is changed,
   --     or DDS_RETCODE_INCONSISTENT_POLICY if Policies Are Inconsistent
   --  See also:
   --     DDS_DomainParticipantFactoryQos for rules on consistency among QoS

   procedure Get_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.DomainParticipantFactoryQos);

   procedure Finalize_Instance (Self : not null access Ref);


private

   type Ref is new RTI.Obj_Impl.Ref with null record;
   TheParticipantFactoryImpl : aliased Ref;

   procedure Initialize;

end DDS.DomainParticipantFactory;
