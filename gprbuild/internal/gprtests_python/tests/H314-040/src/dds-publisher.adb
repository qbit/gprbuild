pragma Ada_05;

package body DDS.Publisher is

   C_DATAWRITER_QOS_DEFAULT : System.Address;
   pragma Interface (C, C_DATAWRITER_QOS_DEFAULT, "DDS_DATAWRITER_QOS_DEFAULT");

   C_DATAWRITER_QOS_USE_TOPIC_QOS : System.Address;
   pragma Interface (C, C_DATAWRITER_QOS_USE_TOPIC_QOS, "DDS_DATAWRITER_QOS_USE_TOPIC_QOS");

   procedure C_DataWriter_Qos_Copy (Dst_Qos_Ptr : System.Address;
                                    Src_Qos_Ptr : System.Address);
   pragma Import (C, C_DataWriter_Qos_Copy, "DDS_DataWriterQos_copy");

   procedure Initialize is
   begin
      C_DataWriter_Qos_Copy (DATAWRITER_QOS_DEFAULT.GetInterface,
                             C_DATAWRITER_QOS_DEFAULT'Address);
      C_DataWriter_Qos_Copy (DATAWRITER_QOS_USE_TOPIC_QOS.GetInterface,
                             C_DATAWRITER_QOS_USE_TOPIC_QOS'Address);
   end Initialize;

begin

   Initialize;

end DDS.Publisher;
