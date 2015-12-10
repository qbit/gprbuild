
--  ===========================================================================
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with DDS.DataReader;
with DDS.DomainParticipant;
with DDS.DomainParticipantFactory;
with DDS.Subscriber;
with DDS.Topic;

with GNAT.Traceback.Symbolic;
--  ===========================================================================


with com.saabgroup.cms.pha.sensor.depth.Depth_SubscriberListener;
with com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport;


procedure  com.saabgroup.cms.pha.sensor.depth.Depth_Subscriber is
   use DDS;

   procedure Subscriber_Shutdown
     (Participant : in out DDS.DomainParticipant.Ref_Access);

   procedure Main (Domain_Id : DDS.DomainId_T; Num_Loops : Integer);

   procedure Subscriber_Shutdown
     (Participant : in out DDS.DomainParticipant.Ref_Access) is
      Factory : constant DDS.DomainParticipantFactory.Ref_Access :=
         DDS.DomainParticipantFactory.Get_Instance;
   begin

      Participant.Delete_Contained_Entities;
      Factory.Delete_Participant (Participant);

      --  RTI Data Distribution Service provides finalize_instance() method for
      --  people who want to release memory used by the participant factory
      --  singleton. Uncomment the following block of code for clean destruction of
      --  the participant factory singleton.
      --  Factory.Finalize_Instance;

   end Subscriber_Shutdown;

   procedure Main (Domain_Id : DDS.DomainId_T; Num_Loops : Integer) is
      Factory           : DDS.DomainParticipantFactory.Ref_Access;
      Participant       : DDS.DomainParticipant.Ref_Access;
      Subscriber        : DDS.Subscriber.Ref_Access;
      Topic             : DDS.Topic.Ref_Access;
      Reader            : DDS.DataReader.Ref_Access;
      Reader_Listener   : aliased Depth_SubscriberListener.My_Listener;
      Type_Name         : DDS.String;
      Poll_Period       : constant Duration := 4.0;

   begin

      Factory := DDS.DomainParticipantFactory.Get_Instance;
      --  To customize participant QoS, use
      --  Factory.get_default_participant_qos
      Put_Line ("Create_Participant:");
      Participant := Factory.Create_Participant
        (Domain_Id,
         DDS.DomainParticipantFactory.PARTICIPANT_QOS_DEFAULT,
         null,
         DDS.STATUS_MASK_NONE);


      --  To customize subscriber QoS, use
      --  DDS.DomainParticipant.get_default_subscriber_qos

      Put_Line ("Create_Subscriber:");
      Subscriber := Participant.Create_Subscriber
        (DDS.DomainParticipant.SUBSCRIBER_QOS_DEFAULT,
         null,
         DDS.STATUS_MASK_NONE);

      --  Register type before creating topic
      Type_Name := Depth_TypeSupport.Get_Type_Name;
      Put_Line ("Register_Type:");
      Depth_TypeSupport.Register_Type (Participant, Type_Name);

      --  To customize topic QoS, use
      --  DDS.DomainParticipant.get_default_topic_qos
      Put_Line ("Create_Topic:");
      Topic := Participant.Create_Topic
        (DDS.To_DDS_String ("Example Depth"),
         Type_Name,
         DDS.DomainParticipant.TOPIC_QOS_DEFAULT,
         null,
         DDS.STATUS_MASK_NONE);

      --  To customize data reader QoS, use
      --  DDS_Subscriber_get_default_datareader_qos()
      Put ("Create_DataReader:");
      Reader := Subscriber.Create_DataReader
        (Topic,
         DDS.Subscriber.DATAREADER_QOS_DEFAULT,
         Reader_Listener'Unchecked_Access,
         DDS.STATUS_MASK_ALL);


      --  Main loop
      for I in 0 .. Num_Loops loop
         Put_Line ("Depth subscriber sleeping " & Poll_Period'Img & " sec.");
         delay Poll_Period;
      end loop;
      --  Cleanup and delete delete all entities
      Subscriber_Shutdown (Participant);
   exception
      when others =>
         --  Cleanup and delete delete all entities
         Subscriber_Shutdown (Participant);
         raise;
   end Main;

begin
   declare
      Num_Loops    : constant Integer := 1000;
      Domain_Id    : constant DDS.DomainId_T := 0;
   begin
      Main (Domain_Id, Num_Loops);
   end;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end com.saabgroup.cms.pha.sensor.depth.Depth_Subscriber;

