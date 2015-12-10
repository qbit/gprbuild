pragma Ada_05;

with DDS.DataReader;
with DDS.DataReader_Impl;

package body DDS.DataReaderListener.Low_Level is

   ----------------------------------
   -- on_requested_deadline_missed --
   ----------------------------------

   procedure On_Requested_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedDeadlineMissedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      R_Listener : constant DDS.DataReaderListener.Ref_Access :=
        DDS.DataReaderListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      R_Listener.On_Requested_Deadline_Missed (Reader, Status);
   end On_Requested_Deadline_Missed;

   -----------------------------------
   -- on_requested_incompatible_qos --
   -----------------------------------

   procedure On_Requested_Incompatible_Qos
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedIncompatibleQosStatus) is
      Reader : DDS.DataReader.Ref_Access;
      R_Listener : constant DDS.DataReaderListener.Ref_Access :=
        DDS.DataReaderListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      R_Listener.On_Requested_Incompatible_Qos (Reader, Status);
   end On_Requested_Incompatible_Qos;

   ------------------------
   -- on_sample_rejected --
   ------------------------

   procedure On_Sample_Rejected
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleRejectedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      R_Listener : constant DDS.DataReaderListener.Ref_Access :=
        DDS.DataReaderListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      R_Listener.On_Sample_Rejected (Reader, Status);
   end On_Sample_Rejected;

   ---------------------------
   -- on_liveliness_changed --
   ---------------------------

   procedure On_Liveliness_Changed
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.LivelinessChangedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      R_Listener : constant DDS.DataReaderListener.Ref_Access :=
        DDS.DataReaderListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      R_Listener.On_Liveliness_Changed (Reader, Status);
   end On_Liveliness_Changed;

   -----------------------
   -- on_data_available --
   -----------------------

   procedure On_Data_Available
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address) is
      Reader : DDS.DataReader.Ref_Access;
      R_Listener : constant DDS.DataReaderListener.Ref_Access :=
        DDS.DataReaderListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      R_Listener.On_Data_Available (Reader);
   end On_Data_Available;

   -----------------------------
   -- on_subscription_matched --
   -----------------------------

   procedure On_Subscription_Matched
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SubscriptionMatchedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      R_Listener : constant DDS.DataReaderListener.Ref_Access :=
        DDS.DataReaderListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      R_Listener.On_Subscription_Matched (Reader, Status);
   end On_Subscription_Matched;

   --------------------
   -- on_sample_lost --
   --------------------

   procedure On_Sample_Lost
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleLostStatus) is
      Reader : DDS.DataReader.Ref_Access;
      R_Listener : constant DDS.DataReaderListener.Ref_Access :=
        DDS.DataReaderListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      R_Listener.On_Sample_Lost (Reader, Status);
   end On_Sample_Lost;

end DDS.DataReaderListener.Low_Level;
