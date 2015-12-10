pragma Ada_05;

with DDS.Listener;
limited with DDS.DataWriter;

package DDS.DataWriterListener is

   type Ref is limited interface and DDS.Listener.Ref;
   type Ref_Access is access all Ref'Class;

   procedure On_Offered_Deadline_Missed
     (Self   : not null access Ref;
      Writer : access DDS.DataWriter.Ref'Class;
      Status : in DDS.OfferedDeadlineMissedStatus) is null;

   procedure On_Offered_Incompatible_Qos
     (Self   : not null access Ref;
      Writer : access DDS.DataWriter.Ref'Class;
      Status : in DDS.OfferedIncompatibleQosStatus) is null;

   procedure On_Liveliness_Lost
     (Self   : not null access Ref;
      Writer : access DDS.DataWriter.Ref'Class;
      Status : in DDS.LivelinessLostStatus) is null;

   procedure On_Publication_Matched
     (Self   : not null access Ref;
      Writer : access DDS.DataWriter.Ref'Class;
      Status : in DDS.PublicationMatchedStatus) is null;

   procedure On_Reliable_Writer_Cache_Changed
     (Self   : not null access Ref;
      Writer : access DDS.DataWriter.Ref'Class;
      Status : in DDS.ReliableWriterCacheChangedStatus) is null;

   procedure On_Reliable_Reader_Activity_Changed
     (Self   : not null access Ref;
      Writer : access DDS.DataWriter.Ref'Class;
      Status : in DDS.ReliableReaderActivityChangedStatus) is null;

end DDS.DataWriterListener;
