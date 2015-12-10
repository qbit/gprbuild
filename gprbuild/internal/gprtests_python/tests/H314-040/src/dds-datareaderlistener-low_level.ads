pragma Ada_05;

with DDS.Listener.Low_Level;
with System;

package DDS.DataReaderListener.Low_Level is

   type T_Requested_Deadline_Missed is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Reader      : System.Address;
      Status        : in DDS.RequestedDeadlineMissedStatus);
   pragma Convention (C, T_Requested_Deadline_Missed);

   type T_Requested_Incompatible_Qos is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Reader      : System.Address;
      Status        : in DDS.RequestedIncompatibleQosStatus);
   pragma Convention (C, T_Requested_Incompatible_Qos);

   type T_Sample_Rejected is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Reader      : System.Address;
      Status        : in DDS.SampleRejectedStatus);
   pragma Convention (C, T_Sample_Rejected);

   type T_Liveliness_Changed is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Reader      : System.Address;
      Status        : in DDS.LivelinessChangedStatus);
   pragma Convention (C, T_Liveliness_Changed);

   type T_Data_Available is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Reader      : System.Address);
   pragma Convention (C, T_Data_Available);

   type T_Subscription_Matched is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Reader      : System.Address;
      Status        : in DDS.SubscriptionMatchedStatus);
   pragma Convention (C, T_Subscription_Matched);

   type T_Sample_Lost is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Reader      : System.Address;
      Status        : in DDS.SampleLostStatus);
   pragma Convention (C, T_Sample_Lost);


   procedure On_Requested_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedDeadlineMissedStatus);
   pragma Convention (C, On_Requested_Deadline_Missed);

   procedure On_Requested_Incompatible_Qos
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedIncompatibleQosStatus);
   pragma Convention (C, On_Requested_Incompatible_Qos);

   procedure On_Sample_Rejected
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleRejectedStatus);
   pragma Convention (C, On_Sample_Rejected);

   procedure On_Liveliness_Changed
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.LivelinessChangedStatus);
   pragma Convention (C, On_Liveliness_Changed);

   procedure On_Data_Available
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address);
   pragma Convention (C, On_Data_Available);

   procedure On_Subscription_Matched
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SubscriptionMatchedStatus);
   pragma Convention (C, On_Subscription_Matched);

   procedure On_Sample_Lost
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleLostStatus);
   pragma Convention (C, On_Sample_Lost);

   type C_DataReaderListener is record
      Listener                   : DDS.Listener.Low_Level.C_Listener;
      Requested_Deadline_Missed  : T_Requested_Deadline_Missed;
      Requested_Incompatible_Qos : T_Requested_Incompatible_Qos;
      Sample_Rejected            : T_Sample_Rejected;
      Liveliness_Changed         : T_Liveliness_Changed;
      Data_Available             : T_Data_Available;
      Subscription_Matched       : T_Subscription_Matched;
      Sample_Lost                : T_Sample_Lost;
   end record;
   pragma Convention (C, C_DataReaderListener);

   C_DataReaderListener_DEFAULT : constant C_DataReaderListener :=
     (Listener                   => (Listener_Data => System.Null_Address),
      Requested_Deadline_Missed  => On_Requested_Deadline_Missed'Access,
      Requested_Incompatible_Qos => On_Requested_Incompatible_Qos'Access,
      Sample_Rejected            => On_Sample_Rejected'Access,
      Liveliness_Changed         => On_Liveliness_Changed'Access,
      Data_Available             => On_Data_Available'Access,
      Subscription_Matched       => On_Subscription_Matched'Access,
      Sample_Lost                => On_Sample_Lost'Access);

end DDS.DataReaderListener.Low_Level;
