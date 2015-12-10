pragma Ada_05;

with DDS.Listener;
limited with DDS.DataReader;

package DDS.DataReaderListener is

   type Ref is limited interface and DDS.Listener.Ref;
   type Ref_Access is access all Ref'Class;

   procedure On_Requested_Deadline_Missed
     (Self       : not null access Ref;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.RequestedDeadlineMissedStatus)
   is null;

   procedure On_Requested_Incompatible_Qos
     (Self       : not null access Ref;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.RequestedIncompatibleQosStatus)
   is null;

   procedure On_Sample_Rejected
     (Self       : not null access Ref;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.SampleRejectedStatus)
   is null;

   procedure On_Liveliness_Changed
     (Self       : not null access Ref;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.LivelinessChangedStatus)
   is null;

   procedure On_Data_Available
     (Self       : not null access Ref;
      The_Reader : access DDS.DataReader.Ref'Class)
   is null;

   procedure On_Subscription_Matched
     (Self       : not null access Ref;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.SubscriptionMatchedStatus)
   is null;

   procedure On_Sample_Lost
     (Self       : not null access Ref;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.SampleLostStatus)
   is null;
end DDS.DataReaderListener;
