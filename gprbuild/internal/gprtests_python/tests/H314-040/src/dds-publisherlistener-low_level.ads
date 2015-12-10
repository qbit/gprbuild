pragma Ada_05;

with System;
with DDS.Listener;
with DDS.DataWriterListener.Low_Level;

package DDS.PublisherListener.Low_Level is

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

   type C_PublisherListener is record
      As_DataWriterListener :
        DataWriterListener.Low_Level.C_DataWriterListener;
   end record;
   pragma Convention (C, C_PublisherListener);

   C_PublisherListener_DEFAULT : constant C_PublisherListener :=
     (As_DataWriterListener =>
        (Listener                      => (Listener_Data => System.Null_Address),
         Offered_Deadline_Missed       => On_Offered_Deadline_Missed'Access,
         Offered_Incompatible_Qos      => On_Offered_Incompatible_Qos'Access,
         Liveliness_Lost               => On_Liveliness_Lost'Access,
         Publication_Matched           => On_Publication_Matched'Access,
         ReliableWriterCacheChanged    => On_Reliable_Writer_Cache_Changed'Access,
         ReliableReaderActivityChanged => On_Reliable_Reader_Activity_Changed'Access));

end DDS.PublisherListener.Low_Level;
