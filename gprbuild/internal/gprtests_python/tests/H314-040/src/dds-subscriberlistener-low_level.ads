pragma Ada_05;

with System;
with DDS.Listener;
with DDS.DataReaderListener.Low_Level;

package DDS.SubscriberListener.Low_Level is

   type T_Data_On_Readers is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Subscriber  : System.Address);
   pragma Convention (C, T_Data_On_Readers);


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


   type C_SubscriberListener is record
      As_DataReaderListener      :
        DataReaderListener.Low_Level.C_DataReaderListener;
      Data_On_Readers            : T_Data_On_Readers;
   end record;
   pragma Convention (C, C_SubscriberListener);

   C_SubscriberListener_DEFAULT : constant C_SubscriberListener :=
     (As_DataReaderListener      =>
        (Listener                   => (Listener_Data => System.Null_Address),
         Requested_Deadline_Missed  => On_Requested_Deadline_Missed'Access,
         Requested_Incompatible_Qos => On_Requested_Incompatible_Qos'Access,
         Sample_Rejected            => On_Sample_Rejected'Access,
         Liveliness_Changed         => On_Liveliness_Changed'Access,
         Data_Available             => On_Data_Available'Access,
         Subscription_Matched       => On_Subscription_Matched'Access,
         Sample_Lost                => On_Sample_Lost'Access),
      Data_On_Readers            => On_Data_On_Readers'Access);

end DDS.SubscriberListener.Low_Level;
