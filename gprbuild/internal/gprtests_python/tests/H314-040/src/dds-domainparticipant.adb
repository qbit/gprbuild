pragma Ada_05;

package body DDS.DomainParticipant is

   C_TOPIC_QOS_DEFAULT : System.Address;
   pragma Interface (C, C_TOPIC_QOS_DEFAULT, "DDS_TOPIC_QOS_DEFAULT");

   C_PUBLISHER_QOS_DEFAULT : System.Address;
   pragma Interface (C, C_PUBLISHER_QOS_DEFAULT, "DDS_PUBLISHER_QOS_DEFAULT");

   C_SUBSCRIBER_QOS_DEFAULT : System.Address;
   pragma Interface (C, C_SUBSCRIBER_QOS_DEFAULT, "DDS_SUBSCRIBER_QOS_DEFAULT");

   procedure C_TopicQos_Copy (Dst_Qos_Ptr : System.Address;
                              Src_Qos_Ptr : System.Address);
   pragma Import (C, C_TopicQos_Copy, "DDS_TopicQos_copy");

   procedure C_PublisherQos_Copy (Dst_Qos_Ptr : System.Address;
                                  Src_Qos_Ptr : System.Address);
   pragma Import (C, C_PublisherQos_Copy, "DDS_PublisherQos_copy");

   procedure C_SubscriberQos_Copy (Dst_Qos_Ptr : System.Address;
                                   Src_Qos_Ptr : System.Address);
   pragma Import (C, C_SubscriberQos_Copy, "DDS_SubscriberQos_copy");

   procedure Initialize is
   begin
      C_TopicQos_Copy (TOPIC_QOS_DEFAULT.GetInterface,
                       C_TOPIC_QOS_DEFAULT'Address);
      C_PublisherQos_Copy (PUBLISHER_QOS_DEFAULT.GetInterface,
                           C_PUBLISHER_QOS_DEFAULT'Address);
      C_SubscriberQos_Copy (SUBSCRIBER_QOS_DEFAULT.GetInterface,
                            C_SUBSCRIBER_QOS_DEFAULT'Address);
   end Initialize;

begin

   Initialize;

end DDS.DomainParticipant;
