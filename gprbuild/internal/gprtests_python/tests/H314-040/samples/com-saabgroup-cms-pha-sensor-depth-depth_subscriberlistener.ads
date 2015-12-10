
with DDS.DataReaderListener;
with DDS.DataReader;

package com.saabgroup.cms.pha.sensor.depth.Depth_SubscriberListener is
   type My_Listener is new DDS.DataReaderListener.Ref with null record;
   type My_Listener_Access is access all My_Listener'Class;

   procedure On_Data_Available
     (Self       : not null access My_Listener;
      The_Reader : access DDS.DataReader.Ref'Class);

   procedure On_Subscription_Matched
     (Self       : not null access My_Listener;
      The_Reader : access constant DDS.DataReader.Ref'Class'Class;
      Status     : in DDS.SubscriptionMatchedStatus)
   is null;

   procedure On_Sample_Lost
     (Self       : not null access My_Listener;
      The_Reader : access constant DDS.DataReader.Ref'Class'Class;
      Status     : in DDS.SampleLostStatus)
   is null;

   procedure On_Requested_Deadline_Missed
     (Self       : not null access My_Listener;
      The_Reader : access constant DDS.DataReader.Ref'Class'Class;
      Status     : in DDS.RequestedDeadlineMissedStatus)
   is null;

   procedure On_Requested_Incompatible_Qos
     (Self       : not null access My_Listener;
      The_Reader : access constant DDS.DataReader.Ref'Class'Class;
      Status     : in DDS.RequestedIncompatibleQosStatus)
   is null;

   procedure On_Sample_Rejected
     (Self       : not null access My_Listener;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.SampleRejectedStatus)
   is null;

   procedure On_Liveliness_Changed
     (Self       : not null access My_Listener;
      The_Reader : access constant DDS.DataReader.Ref'Class;
      Status     : in DDS.LivelinessChangedStatus)
   is null;

end com.saabgroup.cms.pha.sensor.depth.Depth_SubscriberListener;

