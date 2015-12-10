pragma Ada_05;
with DDS.DataReaderListener;
limited with DDS.Subscriber;

package DDS.SubscriberListener is

   type Ref is limited interface and DDS.DataReaderListener.Ref;
   type Ref_Access is access all Ref'Class;

   procedure On_Data_On_Readers
     (Self           : not null access Ref;
      The_Subscriber : access DDS.Subscriber.Ref'Class) is null;

end DDS.SubscriberListener;
