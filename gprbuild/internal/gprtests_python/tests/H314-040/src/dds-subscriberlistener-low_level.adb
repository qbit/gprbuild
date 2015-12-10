pragma Ada_05;

with DDS.DataReader;
with DDS.DataReader_Impl;
with DDS.Subscriber;
with DDS.Subscriber_Impl;

package body DDS.SubscriberListener.Low_Level is

   procedure On_Requested_Deadline_Missed
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedDeadlineMissedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Requested_Deadline_Missed (Reader, Status);
   end On_Requested_Deadline_Missed;

   procedure On_Requested_Incompatible_Qos
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.RequestedIncompatibleQosStatus) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Requested_Incompatible_Qos (Reader, Status);
   end On_Requested_Incompatible_Qos;

   procedure On_Sample_Rejected
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleRejectedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Sample_Rejected (Reader, Status);
   end On_Sample_Rejected;

   procedure On_Liveliness_Changed
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.LivelinessChangedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Liveliness_Changed (Reader, Status);
   end On_Liveliness_Changed;

   procedure On_Data_Available
     (Listener :  DDS.Listener.Ref_Access;
      C_Reader : System.Address) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Data_Available (Reader);
   end On_Data_Available;

   procedure On_Subscription_Matched
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SubscriptionMatchedStatus) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Subscription_Matched (Reader, Status);
   end On_Subscription_Matched;

   procedure On_Sample_Lost
     (Listener : DDS.Listener.Ref_Access;
      C_Reader : System.Address;
      Status   : in DDS.SampleLostStatus) is
      Reader : DDS.DataReader.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Reader := DDS.DataReader.Ref_Access
        (DDS.DataReader_Impl.Get_FacadeI (C_Reader));
      S_Listener.On_Sample_Lost (Reader, Status);
   end On_Sample_Lost;

   procedure On_Data_On_Readers
     (Listener     : DDS.Listener.Ref_Access;
      C_Subscriber : System.Address) is
      Subscriber : DDS.Subscriber.Ref_Access;
      S_Listener : constant DDS.SubscriberListener.Ref_Access :=
        DDS.SubscriberListener.Ref_Access (Listener);
   begin
      Subscriber := DDS.Subscriber.Ref_Access
        (DDS.Subscriber_Impl.Get_FacadeI (C_Subscriber));
      S_Listener.On_Data_On_Readers (Subscriber);
   end On_Data_On_Readers;

end DDS.SubscriberListener.Low_Level;
