        
Package FC_Manager is

   type Input_Message_Type is 
      ( InitializeNIC,
        Map_Device,
        Poll_Fibre_Service,  
        RDMATransmitRequest,
        RDMAReceiveRequest,
        Update_Local_DMA );
   
               
   type Port_Status_Type is
      ( Ready_For_Initialization,
        Comm_Lost,
        Commanded_Init,
        Uncommanded_Init,
        Self_Test_Fail,
        Fibre_Init_Fail,
        Fibre_Init_Success,
        Ready_For_Data_Transfer,
        Port_Shutdown,
        Fibrechannel_System_Error );

   Procedure Dummy;

        
End FC_Manager;
