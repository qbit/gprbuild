pragma Ada_05;

with DDS.TopicListener;
with DDS.SubscriberListener;
with DDS.PublisherListener;

package DDS.DomainParticipantListener is

   type Ref is limited interface and SubscriberListener.Ref
     and DDS.PublisherListener.Ref and DDS.TopicListener.Ref;
   type Ref_Access is access all Ref'Class;

end DDS.DomainParticipantListener;
