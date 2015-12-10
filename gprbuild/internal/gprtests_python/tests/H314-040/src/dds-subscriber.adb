pragma Ada_05;

package body DDS.Subscriber is

   C_DATAREADER_QOS_DEFAULT : System.Address;
   pragma Import (C, C_DATAREADER_QOS_DEFAULT, "DDS_DATAREADER_QOS_DEFAULT");

   C_DATAREADER_QOS_USE_TOPIC_QOS : System.Address;
   pragma Import (C, C_DATAREADER_QOS_USE_TOPIC_QOS, "DDS_DATAREADER_QOS_USE_TOPIC_QOS");

   procedure C_DataReader_Qos_Copy (Dst_Qos_Ptr : System.Address;
                                    Src_Qos_Ptr : System.Address);
   pragma Import (C, C_DataReader_Qos_Copy, "DDS_DataReaderQos_copy");

   procedure Initialize is
   begin
      C_DataReader_Qos_Copy (DATAREADER_QOS_DEFAULT.GetInterface,
                             C_DATAREADER_QOS_DEFAULT'Address);
      C_DataReader_Qos_Copy (DATAREADER_QOS_USE_TOPIC_QOS.GetInterface,
                             C_DATAREADER_QOS_USE_TOPIC_QOS'Address);
   end Initialize;

begin
   Initialize;
end DDS.Subscriber;
