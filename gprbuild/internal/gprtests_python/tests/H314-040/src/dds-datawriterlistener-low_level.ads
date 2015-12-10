pragma Ada_05;

with DDS.Listener.Low_Level;
with System;

package DDS.DataWriterListener.Low_Level is

   type T_Offered_Deadline_Missed is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Writer      : System.Address;
      Status        : in DDS.OfferedDeadlineMissedStatus);
   pragma Convention (C, T_Offered_Deadline_Missed);

   type T_Offered_Incompatible_Qos is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Writer      : System.Address;
      Status        : in DDS.OfferedIncompatibleQosStatus);
   pragma Convention (C, T_Offered_Incompatible_Qos);

   type T_Liveliness_Lost is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Writer      : System.Address;
      Status        : in DDS.LivelinessLostStatus);
   pragma Convention (C, T_Liveliness_Lost);

   type T_Publication_Matched is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Writer      : System.Address;
      Status        : in DDS.PublicationMatchedStatus);
   pragma Convention (C, T_Publication_Matched);

   type T_ReliableWriterCacheChanged is access procedure
     (Listener_Data  : DDS.Listener.Ref_Access;
      C_Writer       : System.Address;
      Status         : in DDS.ReliableWriterCacheChangedStatus);
   pragma Convention (C, T_ReliableWriterCacheChanged);

   type T_ReliableReaderActivityChanged is access procedure
     (Listener_Data : DDS.Listener.Ref_Access;
      C_Writer      : System.Address;
      Status        : in DDS.ReliableReaderActivityChangedStatus);
   pragma Convention (C, T_ReliableReaderActivityChanged);



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

   type C_DataWriterListener is record
      Listener                      : DDS.Listener.Low_Level.C_Listener;
      Offered_Deadline_Missed       : T_Offered_Deadline_Missed;
      Offered_Incompatible_Qos      : T_Offered_Incompatible_Qos;
      Liveliness_Lost               : T_Liveliness_Lost;
      Publication_Matched           : T_Publication_Matched;
      ReliableWriterCacheChanged    : T_ReliableWriterCacheChanged;
      ReliableReaderActivityChanged : T_ReliableReaderActivityChanged;
   end record;
   pragma Convention (C, C_DataWriterListener);

   C_DataWriterListener_DEFAULT : constant C_DataWriterListener :=
     (Listener                      => (Listener_Data => System.Null_Address),
      Offered_Deadline_Missed       => On_Offered_Deadline_Missed'Access,
      Offered_Incompatible_Qos      => On_Offered_Incompatible_Qos'Access,
      Liveliness_Lost               => On_Liveliness_Lost'Access,
      Publication_Matched           => On_Publication_Matched'Access,
      ReliableWriterCacheChanged    => On_Reliable_Writer_Cache_Changed'Access,
      ReliableReaderActivityChanged => On_Reliable_Reader_Activity_Changed'Access);

end DDS.DataWriterListener.Low_Level;
