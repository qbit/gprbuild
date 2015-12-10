pragma Ada_05;

with System;
with DDS.Listener;
with DDS.TopicListener.Low_Level;
with DDS.PublisherListener.Low_Level;
with DDS.SubscriberListener.Low_Level;

package DDS.DomainParticipantListener.Low_Level is

   --  Topic
   procedure On_Inconsistent_Topic
     (Listener : DDS.Listener.Ref_Access;
      C_Topic  : System.Address;
      Status   : in DDS.InconsistentTopicStatus);
   pragma Convention (C, On_Inconsistent_Topic);

   --  Publisher
   procedure On_Offered_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.OfferedDeadlineMissedStatus);
   pragma Convention (C, On_Offered_Deadline_Missed);

   procedure On_Offered_Incompatible_Qos
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.OfferedIncompatibleQosStatus);
   pragma Convention (C, On_Offered_Incompatible_Qos);

   procedure On_Liveliness_Lost
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.LivelinessLostStatus);
   pragma Convention (C, On_Liveliness_Lost);

   procedure On_Publication_Matched
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.PublicationMatchedStatus);
   pragma Convention (C, On_Publication_Matched);

   procedure On_Reliable_Writer_Cache_Changed
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.ReliableWriterCacheChangedStatus);
   pragma Convention (C, On_Reliable_Writer_Cache_Changed);

   procedure On_Reliable_Reader_Activity_Changed
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.ReliableReaderActivityChangedStatus);
   pragma Convention (C, On_Reliable_Reader_Activity_Changed);

   --  Subscriber
   procedure On_Requested_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedDeadlineMissedStatus);
   pragma Convention (C, On_Requested_Deadline_Missed);

   procedure On_Requested_Incompatible_Qos
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedIncompatibleQosStatus);
   pragma Convention (C, On_Requested_Incompatible_Qos);

   procedure On_Sample_Rejected
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleRejectedStatus);
   pragma Convention (C, On_Sample_Rejected);

   procedure On_Liveliness_Changed
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.LivelinessChangedStatus);
   pragma Convention (C, On_Liveliness_Changed);

   procedure On_Data_Available
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address);
   pragma Convention (C, On_Data_Available);

   procedure On_Subscription_Matched
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SubscriptionMatchedStatus);
   pragma Convention (C, On_Subscription_Matched);

   procedure On_Sample_Lost
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleLostStatus);
   pragma Convention (C, On_Sample_Lost);

   procedure On_Data_On_Readers
     (Listener     : DDS.Listener.Ref_Access;
      C_Subscriber : System.Address);
   pragma Convention (C, On_Data_On_Readers);


   type C_DomainParticipantListener is record
      As_TopicListener :
        TopicListener.Low_Level.C_TopicListener;
      As_PublisherListener :
        PublisherListener.Low_Level.C_PublisherListener;
      As_SubscriberListener :
        SubscriberListener.Low_Level.C_SubscriberListener;
   end record;
   pragma Convention (C, C_DomainParticipantListener);

   C_DomainParticipantListener_DEFAULT : constant C_DomainParticipantListener :=
     (As_TopicListener =>
        (Listener            => (Listener_Data => System.Null_Address),
         Inconsistent_Topic  => On_Inconsistent_Topic'Access),
      As_PublisherListener   =>
        (As_DataWriterListener      =>
           (Listener                      => (Listener_Data => System.Null_Address),
            Offered_Deadline_Missed       => On_Offered_Deadline_Missed'Access,
            Offered_Incompatible_Qos      => On_Offered_Incompatible_Qos'Access,
            Liveliness_Lost               => On_Liveliness_Lost'Access,
            Publication_Matched           => On_Publication_Matched'Access,
            ReliableWriterCacheChanged    => On_Reliable_Writer_Cache_Changed'Access,
            ReliableReaderActivityChanged => On_Reliable_Reader_Activity_Changed'Access)),
      As_SubscriberListener   =>
        (As_DataReaderListener      =>
           (Listener                   => (Listener_Data => System.Null_Address),
            Requested_Deadline_Missed  => On_Requested_Deadline_Missed'Access,
            Requested_Incompatible_Qos => On_Requested_Incompatible_Qos'Access,
            Sample_Rejected            => On_Sample_Rejected'Access,
            Liveliness_Changed         => On_Liveliness_Changed'Access,
            Data_Available             => On_Data_Available'Access,
            Subscription_Matched       => On_Subscription_Matched'Access,
            Sample_Lost                => On_Sample_Lost'Access),
         Data_On_Readers            => On_Data_On_Readers'Access));

end DDS.DomainParticipantListener.Low_Level;
