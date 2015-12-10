pragma Ada_05;

with DDS.Topic;
with DDS.Topic_Impl;
with DDS.DataWriter;
with DDS.DataWriter_Impl;
with DDS.DataReader;
with DDS.DataReader_Impl;
with DDS.Subscriber;
with DDS.Subscriber_Impl;

package body DDS.DomainParticipantListener.Low_Level is

   --  Topic
   procedure On_Inconsistent_Topic
     (Listener : DDS.Listener.Ref_Access;
      C_Topic  : System.Address;
      Status   : in DDS.InconsistentTopicStatus) is
      Topic : DDS.Topic.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Topic := DDS.Topic.Ref_Access
        (DDS.Topic_Impl.Get_FacadeI (C_Topic));
      P_Listener.On_Inconsistent_Topic (Topic, Status);
   end On_Inconsistent_Topic;

   --  Publisher
   procedure On_Offered_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.OfferedDeadlineMissedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      P_Listener.On_Offered_Deadline_Missed (Writer, Status);
   end On_Offered_Deadline_Missed;

   procedure On_Offered_Incompatible_Qos
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.OfferedIncompatibleQosStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      P_Listener.On_Offered_Incompatible_Qos (Writer, Status);
   end On_Offered_Incompatible_Qos;

   procedure On_Liveliness_Lost
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.LivelinessLostStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      P_Listener.On_Liveliness_Lost (Writer, Status);
   end On_Liveliness_Lost;

   procedure On_Publication_Matched
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.PublicationMatchedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      P_Listener.On_Publication_Matched (Writer, Status);
   end On_Publication_Matched;

   procedure On_Reliable_Writer_Cache_Changed
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.ReliableWriterCacheChangedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      P_Listener.On_Reliable_Writer_Cache_Changed (Writer, Status);
   end On_Reliable_Writer_Cache_Changed;

   procedure On_Reliable_Reader_Activity_Changed
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.ReliableReaderActivityChangedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      P_Listener.On_Reliable_Reader_Activity_Changed (Writer, Status);
   end On_Reliable_Reader_Activity_Changed;

   --  Subscriber
   procedure On_Requested_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedDeadlineMissedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Requested_Deadline_Missed (Reader, Status);
   end On_Requested_Deadline_Missed;

   procedure On_Requested_Incompatible_Qos
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedIncompatibleQosStatus) is
      Reader : DDS.DataReader.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      P_Listener.On_Requested_Incompatible_Qos (Reader, Status);
   end On_Requested_Incompatible_Qos;

   procedure On_Sample_Rejected
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleRejectedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      P_Listener.On_Sample_Rejected (Reader, Status);
   end On_Sample_Rejected;

   procedure On_Liveliness_Changed
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.LivelinessChangedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      P_Listener.On_Liveliness_Changed (Reader, Status);
   end On_Liveliness_Changed;

   procedure On_Data_Available
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address) is
      Reader : DDS.DataReader.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      P_Listener.On_Data_Available (Reader);
   end On_Data_Available;

   procedure On_Subscription_Matched
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SubscriptionMatchedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      P_Listener.On_Subscription_Matched (Reader, Status);
   end On_Subscription_Matched;

   procedure On_Sample_Lost
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleLostStatus) is
      Reader : DDS.DataReader.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      P_Listener.On_Sample_Lost (Reader, Status);
   end On_Sample_Lost;

   procedure On_Data_On_Readers
     (Listener     : DDS.Listener.Ref_Access;
      C_Subscriber : System.Address) is
      Subscriber : DDS.Subscriber.Ref_Access;
      P_Listener : constant DDS.DomainParticipantListener.Ref_Access :=
        DDS.DomainParticipantListener.Ref_Access (Listener);
   begin
      Subscriber := DDS.Subscriber.Ref_Access
        (DDS.Subscriber_Impl.Get_FacadeI (C_Subscriber));
      P_Listener.On_Data_On_Readers (Subscriber);
   end On_Data_On_Readers;

end DDS.DomainParticipantListener.Low_Level;
