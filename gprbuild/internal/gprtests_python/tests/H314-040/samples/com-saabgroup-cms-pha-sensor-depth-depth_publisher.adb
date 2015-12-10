
--  ===========================================================================
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with DDS.DomainParticipant;
with DDS.DomainParticipantFactory;
with DDS.Publisher;
with DDS.Topic;

with GNAT.Traceback.Symbolic;

with Interfaces.C.Strings; use Interfaces.C.Strings;

--   Uncomment this to turn on additional logging
--  with NDDS.Config.Logger;

--  ===========================================================================

with com.saabgroup.cms.pha.sensor.depth.Depth;
with com.saabgroup.cms.pha.sensor.depth.Depth_DataWriter;
with com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport;

procedure com.saabgroup.cms.pha.sensor.depth.Depth_Publisher  is
   use type DDS.ReturnCode_T;
   --  procedure DDS_Enable_All_Traces;
   --  pragma Import (C, DDS_Enable_All_Traces, "DDS_Enable_All_Traces");

   --  Delete all entities
   procedure  Publisher_Shutdown
     (L_Participant : in out DDS.DomainParticipant.Ref_Access) is

      Factory : constant DDS.DomainParticipantFactory.Ref_Access :=
         DDS.DomainParticipantFactory.Get_Instance;
   begin
      L_Participant.Delete_Contained_Entities;
      Factory.Delete_Participant (L_Participant);
      --  RTI Data Distribution Service provides finalize_instance() method for
      --  people who want to release memory used by the participant factory
      --  singleton. Uncomment the following block of code for clean destruction of
      --  the participant factory singleton.

      --  Factory.finalize_instance;

   end Publisher_Shutdown;



   procedure Publisher_Main (DomainId     : DDS.DomainId_T;
                            Sample_Count : Integer) is

      Factory           :  DDS.DomainParticipantFactory.Ref_Access;
      Participant       :  DDS.DomainParticipant.Ref_Access;
      Publisher         :  DDS.Publisher.Ref_Access;
      Topic             :  DDS.Topic.Ref_Access;

      Depth_Writer : Depth_DataWriter.Ref_Access;

      Instance          : Depth.Depth_Access;
      Instance_Handle   : aliased DDS.InstanceHandle_T := DDS.HANDLE_NIL;
      Type_Name         : DDS.String;

      Send_Period       : constant Duration  := 0.5;


      Topic_Name : constant DDS.String := DDS.To_DDS_String ("Example Depth");
   begin

      Factory := DDS.DomainParticipantFactory.Get_Instance;
      --  To customize participant QoS, use
      --  Factory.get_default_participant_qos instead
      Put ("Create_Participant:");
      Participant := Factory.Create_Participant
        (DomainId,
         DDS.DomainParticipantFactory.PARTICIPANT_QOS_DEFAULT,
         null,
         DDS.STATUS_MASK_NONE);

      --  To customize publisher QoS, use
      --  DDS.DomainParticipant.get_default_publisher_qos instead


      Put ("Create_Publisher:");
      Publisher := Participant.Create_Publisher
        (DDS.DomainParticipant.PUBLISHER_QOS_DEFAULT,
         null,
         DDS.STATUS_MASK_NONE);



      --  Register type before creating topic
      Type_Name := Depth_TypeSupport.Get_Type_Name;
      Depth_TypeSupport.Register_Type (Participant, Type_Name);

      --  To customize topic QoS, use
      --  DDS.DomainParticipant.get_default_topic_qos instead
      Put ("Create_Topic:");
      Topic := Participant.Create_Topic
        (TOPIC_NAME,
         Type_Name,
         DDS.DomainParticipant.TOPIC_QOS_DEFAULT,
         null,
         DDS.STATUS_MASK_NONE);


      --  To customize data writer QoS, use
      --  DDS.Publisher.get_default_datawriter_qos instead.
      Put ("Create_DataWriter:");
      Depth_Writer := Depth_DataWriter.Ref_Access
        (Publisher.Create_DataWriter (Topic,
         DDS.Publisher.DATAWRITER_QOS_DEFAULT,
         null,
         DDS.STATUS_MASK_NONE));

      Instance := Depth_TypeSupport.Create_Data (TRUE);

      --  For data type that has key, if the same instance is going to be
      --  written multiple times, initialize the key here
      --  and register the keyed instance prior to writing
      Instance_Handle := Depth_Writer.Register_Instance (Instance);

      --  Main loop
      for Count in 0 .. Sample_Count loop

         Put_Line ("Writing Depth, count " &  Count'Img);
         pragma Compile_Time_Warning (True, "Complete update of message.");
         --  declare
         --     Msg : constant Standard.String := " Count=" & Integer'Image (Count);
         --  begin
         --     Update (Item   => Instance.Msg,
         --             Offset => 0,
         --             Str  =>  Msg, Check => False);
         --  end;

         Depth_Writer.Write (Instance_Data => Instance,
                                   Handle        => Instance_Handle'Unchecked_Access);


         delay Send_Period;
      end loop;

      Depth_Writer.Unregister_Instance   (Instance, Instance_Handle);

      --   Delete data sample
      Depth_TypeSupport.Delete_Data (Instance, True);

      Publisher_Shutdown (Participant);


   end Publisher_Main;


begin
   Put_Line ("-----------------------------------------------------");
   New_Line (4);
   declare
      DomainId     : DDS.DomainId_T := 0;
      Sample_Count : Integer := Integer'Last; -- almost infinite
   begin

      if Argument_Count >= 1 then
         begin
            DomainId := DDS.DomainId_T'Value (Argument (1));
         exception
            when others => null;
         end;
      end if;

      if Argument_Count >= 2 then
         begin
            Sample_Count := Integer'Value (Argument (2));
         exception
            when others => null;
         end;
      end if;

      --   Uncomment this to turn on additional logging
      --  NDDS.Config.Logger.Get_Instance.Set_Verbosity_By_Category
      --    (NDDS.CONFIG.API,
      --     NDDS.CONFIG.LOG_VERBOSITY_ALL);

      Publisher_Main (DomainId, Sample_Count);
   end;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end com.saabgroup.cms.pha.sensor.depth.Depth_Publisher;

