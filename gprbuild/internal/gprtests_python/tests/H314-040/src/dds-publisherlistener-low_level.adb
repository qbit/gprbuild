pragma Ada_05;

with DDS.DataWriter;
with DDS.DataWriter_Impl;


package body DDS.PublisherListener.Low_Level is

   procedure On_Offered_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Writer : System.Address;
      Status   : in DDS.OfferedDeadlineMissedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      P_Listener : constant DDS.PublisherListener.Ref_Access :=
        DDS.PublisherListener.Ref_Access (Listener);
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
      P_Listener : constant DDS.PublisherListener.Ref_Access :=
        DDS.PublisherListener.Ref_Access (Listener);
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
      P_Listener : constant DDS.PublisherListener.Ref_Access :=
        DDS.PublisherListener.Ref_Access (Listener);
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
      P_Listener : constant DDS.PublisherListener.Ref_Access :=
        DDS.PublisherListener.Ref_Access (Listener);
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
      P_Listener : constant DDS.PublisherListener.Ref_Access :=
        DDS.PublisherListener.Ref_Access (Listener);
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
      P_Listener : constant DDS.PublisherListener.Ref_Access :=
        DDS.PublisherListener.Ref_Access (Listener);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      P_Listener.On_Reliable_Reader_Activity_Changed (Writer, Status);
   end On_Reliable_Reader_Activity_Changed;

end DDS.PublisherListener.Low_Level;
