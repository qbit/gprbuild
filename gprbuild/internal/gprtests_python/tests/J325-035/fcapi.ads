--**********************************************************************
--**                      Northrop Grumman                            **
--**     Information contained herein is proprietary to               **
--**                      Northrop Grumman                            **
--**********************************************************************
--
--                   AAA     SSSSS    CCCCC                      
--                  AA AA   SS   SS  CC   CC            
--                 AA   AA  SS       CC                   
--                 AAAAAAA   SSSSS   CC                  
--                 AA   AA       SS  CC                   
--                 AA   AA  SS   SS  CC   CC                 
--                 AA   AA   SSSSS    CCCCC                
--
--
-- Northrop Grumman Bethpage, New York 11714
--
-- Program      : EA-18 FOSJ
--
-- CSCI         : ASC
--
-- DATE CREATED : 05/19/2004                                                
--
-- AUTHOR       : G. Prete
--
-- PURPOSE      : This package will contain Ada bindings to the Fibre Channel
--                API.
--
--
--
-- MODIFICATION HISTORY
--
--     AUTHOR        :  G. Prete
--     DATE          :  
--     MODIFICATION  :  
--                      
--                      
--                      
--      
-- ------------------------------------------------------------------------ --
-- ------------------------------------------------------------------------ --

With Interfaces.C;
With System;   
With System_Common_Types;   

Package FCAPI is

                   
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
   --                                                                    --
   --          Data Type inputs for procedures and functions.            --
   --          These definitions should mirror the definitions given     --
   --          in FCTAILOR.H                                             --
   --                                                                    --
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
--   Subtype S8              is  Byte_Integer;
   Subtype U8              is  Interfaces.C.Unsigned_Char;
   
   Subtype S16             is  Short_Integer;
   Subtype U16             is  Interfaces.C.Unsigned_Short;
   
   Subtype S32             is  Integer;                                         
   Subtype U32             is  Interfaces.C.Unsigned;
         
   Subtype BaseAddressType is  U32;
   Subtype Fibre_Test_Err  is  U32;

   
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
   --                                                                    --
   --      Configuration Constants should match those in FCAPI.H         --
   --                                                                    --
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
                                            
   -- Number of entries in the active port map returned in Fibre_Status
   Port_Map_Size : constant integer := 128;
   
   -- Status type response info length in bytes  
   FCP_Response_Info_Size : constant integer := 8;

   -- Status type sense data length in bytes
   SCSI_Sense_Data_Size : constant integer := 32;

   -- Size limit (in bytes) for user supplied scsi commands 
   SCSI_Command_Length : constant integer := 16;
                                       
   -- Max Frame Size
   AMCD_Max_Frame_Size : constant integer := 2112;

                           
   
   --------------------------------------------------------------------------
   -- Constants for determining FCRM Working Buffer Size
   --
   MaxConcurrentCommands       : constant integer := 128;   -- From FCTAILOR.C
   MailboxCommandQueueSize     : constant integer := 20;    -- From FCTAILOR.C
   Req_Resp_Queue_Entry_Size   : constant integer := 64;
   Request_Response_Queue_Size : constant integer := MaxConcurrentCommands * 
                                                     Req_Resp_Queue_Entry_Size;
   Mailbox_Command_Buffer_Queue_Size : constant integer := 128;
   Mailbox_Command_Buffer_Base_Size  : constant integer := 8192;
   Mailbox_Command_Buffer_Size       : constant integer :=
      (Mailbox_Command_Buffer_Base_Size +
       MailboxCommandQueueSize * Mailbox_Command_Buffer_Queue_Size);
   
   IP_Rcv_Buf_Queue_Size : constant integer := 0;

   --  
   -- Buffer size for FC-2x00 command and response 
   -- queues and IP rcv buf queue 
   --
   FCRM_Working_Buffer_Size : constant integer :=
      ((2*Request_Response_Queue_Size) + 
       Mailbox_Command_Buffer_Size + IP_Rcv_Buf_Queue_Size);
   --------------------------------------------------------------------------

                                             
                                             
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
   --                                                                    --
   --          Data Types translated from FCAPI.H                        --
   --                                                                    --
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --

   Type FC_ALIAS_STATUS_TYPE is
      ( ALIAS_NON_SPECIFIC_ERROR, 
        ALIAS_LOOPID_USED, 
        ALIAS_ALL_LOOPIDS_USED,
        ALIAS_COMMAND_PARAM_ERROR,
        ALIAS_STATUS_ERROR_4,
        ALIAS_STATUS_ERROR_5,
        ALIAS_STATUS_ERROR_6,
        ALIAS_STATUS_ERROR_7,
        ALIAS_RSVD, 
        ALIAS_NO_LINK, 
        ALIAS_BUFFER_ALLOCATION_ERROR, 
        ALIAS_RESOURCE_ALLOCATION_ERROR, 
        ALIAS_TIMEOUT,
        ALIAS_NO_SWITCH, 
        ALIAS_NO_TGT_SPRT, 
        ALIAS_FW_NOT_READY, 
        ALIAS_INIT_MODE_DSBLD, 
        ALIAS_PORT_NOT_LOGGED_IN, 
        ALIAS_NO_PORT_RESOURCES, 
        ALIAS_DISC_DSBLD, 
        ALIAS_HARD_ID_VALID, 
        ALIAS_NO_XCHG_FND, 
        ALIAS_RCVD_LS_RJT,
        ALIAS_GETLIST_ACTIVE, 
        ALIAS_SCTP_ACTIVE, 
        ALIAS_BLD_XCHG_FAIL, 
        ALIAS_BP_INIT_FAIL, 
        ALIAS_SCTP_DEINIT,
        ALIAS_LINK_BCKUP_FAIL, 
        ALIAS_CMD_FAIL, 
        ALIAS_TOPO_ERR, 
        ALIAS_RESET, 
        ALIAS_ERR
      );

        
   For FC_ALIAS_STATUS_TYPE use     
      ( ALIAS_NON_SPECIFIC_ERROR        =>  0,
        ALIAS_LOOPID_USED               =>  1,
        ALIAS_ALL_LOOPIDS_USED          =>  2,
        ALIAS_COMMAND_PARAM_ERROR       =>  3,
        ALIAS_STATUS_ERROR_4            =>  4,
        ALIAS_STATUS_ERROR_5            =>  5,
        ALIAS_STATUS_ERROR_6            =>  6,
        ALIAS_STATUS_ERROR_7            =>  7,
        ALIAS_RSVD                      =>  8,
        ALIAS_NO_LINK                   =>  9,
        ALIAS_BUFFER_ALLOCATION_ERROR   => 10,
        ALIAS_RESOURCE_ALLOCATION_ERROR => 11,
        ALIAS_TIMEOUT                   => 12,
        ALIAS_NO_SWITCH                 => 13,
        ALIAS_NO_TGT_SPRT               => 14,
        ALIAS_FW_NOT_READY              => 15,
        ALIAS_INIT_MODE_DSBLD           => 16,
        ALIAS_PORT_NOT_LOGGED_IN        => 17,
        ALIAS_NO_PORT_RESOURCES         => 18,
        ALIAS_DISC_DSBLD                => 19,
        ALIAS_HARD_ID_VALID             => 20,
        ALIAS_NO_XCHG_FND               => 21,
        ALIAS_RCVD_LS_RJT               => 22,
        ALIAS_GETLIST_ACTIVE            => 23,
        ALIAS_SCTP_ACTIVE               => 24,
        ALIAS_BLD_XCHG_FAIL             => 25,
        ALIAS_BP_INIT_FAIL              => 26,
        ALIAS_SCTP_DEINIT               => 27,
        ALIAS_LINK_BCKUP_FAIL           => 28,
        ALIAS_CMD_FAIL                  => 29,
        ALIAS_TOPO_ERR                  => 30,
        ALIAS_RESET                     => 31,
        ALIAS_ERR                       => 32
      );


   Type FC_STATUS_TYPE is
      ( FCS_LIP_OCCURRED,
        FCS_LOOP_UP,	
        FCS_LOOP_DOWN,
        FCS_WATCHDOG_TIMEOUT,
        FCS_SYSTEM_ERROR,
        FCS_CONNECTED_POINT_TO_POINT,
        FCS_FABRIC_CHANGE,
        FCS_PORT_ID,
        FCS_OUT_OF_IP_RCV_BUFFERS,
        FCS_LIP_ERROR_OCCURRED,	
        FCS_R_T_TOV, 
        FCS_PLOGI_OCCURRED,
        FCS_PRLI_OCCURRED,
        FCS_PRLI_ACC_OCCURRED,
        FCS_LOGIN_CMPLT, 
        FCS_CSU_OCCURRED, 
        FCS_XFER_ERROR,    
        FCS_PDISC_OCCURRED,
        FCS_PDISC_ACC_OCCURRED,	
        FCS_READY, 
        FCS_BP_ACTIVE,	 
        FCS_LOS,	 
        FCS_DBG_DMP_RCVD, 
        FCS_DBG_TRC_STRT, 
        FCS_DBG_TRC_STP, 
        FCS_AL_TIME, 
        FCS_LP_TOV,	 
        FCS_PORT_DB_CHANGE_OCCURRED,
        FCS_IMMED_NOTIFY_IOCB_RCVD,    --  Added.  New Firmware/API.
                                       --  G. Prete 5-10-06
        FCS_UNHANDLED_EVENT	
      );

   For FC_STATUS_TYPE'size use 32;
   
                                             
   -- ------------------------------------------------------------------ --

   -- Initialization return type
   Type FIBRE_INIT_ERR is
      ( FI_SUCCESS,
        FI_DEVICE_NOT_FOUND,
        FI_DEVICE_NOT_INITIALIZED,
        FI_DEVICE_ALREADY_INITIALIZED,
        FI_KEY_FAIL,
        FI_INVALID_HOST_ID,
        FI_TOO_MANY_ADAPTERS,
        FI_CHECKSUM_FAIL,
        FI_INVALID_FRAME_SIZE,
        FI_INVALID_NUM_SUBADDRESSES,
        FI_INVALID_OMNIPORT_CONFIG,
        FI_DMA_ERROR
      );
      
   For FIBRE_INIT_ERR'size use 32;

   -- ------------------------------------------------------------------ --

   -- Register, Send, and Receive return type  
   Type FIBRE_TRANSFER_ERR is
      ( FT_SUCCESS,
        FT_DEVICE_NOT_INITIALIZED,
        FT_INVALID_HOST_ID,
        FT_INVALID_SUBADDRESS,
        FT_INVALID_PORT,
        FT_INVALID_BUFFER_SIZE,
        FT_INVALID_BUFFER,
        FT_INVALID_PROTOCOL,
        FT_INVALID_DIRECTION,
        FT_INTERNAL_ERROR,
        FT_OUTSTANDING_REQUEST,
        FT_DESCRIPTOR_MISMATCH,
        FT_UNKNOWN_RECORD,
        FT_LOS_STATE,
        FT_PREQUEUED_TARGET_DISABLED,
        FT_ADAPTER_QUEUE_FULL

      );

   For FIBRE_TRANSFER_ERR'size use 32;

   -- ------------------------------------------------------------------ --

   -- Element 0 is MSB of 8-byte word
   Type WorldWideNameType is new string(1..8);
   For  WorldWideNameType'size use 64;
   
   Type Port_Map_Type       is array (0..Port_Map_Size-1) of U8;
   
   Type FC_CONNECTION_TYPE is
      ( FCCT_NOT_VALID,
        FCCT_PRIVATE_LOOP,       -- Arbitrated loop, with no switch
        FCCT_PUBLIC_LOOP,        -- Arbitrated loop, with a switch
        FCCT_NPORT_TO_NPORT_P2P, -- Point to point between two NPorts
        FCCT_NPORT_TO_FPORT_P2P  -- Point to point between a NPort and a Switch
      );
   For FC_CONNECTION_TYPE'size use 32;   

   Type FC_CONNECTION_SPEED_TYPE is
      ( SPEED_1GIG,
        SPEED_2GIG
      );
   For FC_CONNECTION_SPEED_TYPE'size use 32;   

   Type FIBRE_STATUS_TYPE is 
      Record             
         Valid                : Integer;         
         LoopUp               : Integer;       
         LoopTransitionCount  : U8;
         LIPCount             : U8;
         DroppedTransferCount : U16;
         FreeFCRMQueueEntries : U16;
         ConnectionType       : FC_CONNECTION_TYPE;
         ConnectionSpeed      : FC_CONNECTION_SPEED_TYPE;
         IDValid              : Integer;
         LoopID               : U8;
         PortID               : U32;
         PortMapValid         : Integer;
         PortMap              : Port_Map_Type;
         SyncValid            : Integer;
         NumActivePorts       : U8;
      End Record;

   Null_Fibre_Status : constant FIBRE_STATUS_TYPE :=
      (  Valid                => 0,
         LoopUp               => 0,
         LoopTransitionCount  => 0,
         LIPCount             => 0,
         DroppedTransferCount => 0,
         FreeFCRMQueueEntries => 0,
         ConnectionType       => FCCT_NOT_VALID,
         ConnectionSpeed      => SPEED_1GIG,
         IDValid              => 0,
         LoopID               => 0,
         PortID               => 0,
         PortMapValid         => 0,
         PortMap              => (others => 0),
         SyncValid            => 0,
         NumActivePorts       => 0 );
      
      
   -- ------------------------------------------------------------------ --

   Type FC_DIRECTION_TYPE is
      ( FC_SEND,
        FC_SEND_THROTTLED,
        FC_SEND_PREFERENCE,
        FC_RECEIVE
      );
       
   For FC_DIRECTION_TYPE'size use 32;    
         
   For FC_DIRECTION_TYPE use
      ( FC_SEND             =>   0,
        FC_SEND_THROTTLED   =>   1,
        FC_SEND_PREFERENCE  =>   2,
        FC_RECEIVE          => 256
      );
     
   -- ------------------------------------------------------------------ --
        
   Type ProtocolType is
      ( FC_INITIATOR, 
        FC_RDMA_INITIATOR, 
        FC_RDMA_INITIATOR_NO_TARGET_NOTIFICATION,
        FC_RDMA_INITIATOR_MULTICAST,
        FC_RDMA_INITIATOR_MULTICAST_NO_TARGET_NOTIFICATION,
        FC_RDMA_INITIATOR_SG_MULTICAST,
        FC_RDMA_INITIATOR_SG_MULTICAST_NO_TARGET_NOTIFICATION,
        FC_IP_INITIATOR,
        FC_TARGET, 
        FC_IP_TARGET, 
        FC_RDMA_TARGET,
        FC_IP_TARGET_BROADCAST
      );
      
   For ProtocolType'size use 32;

                 
                 
   For ProtocolType use
      ( FC_INITIATOR                                             =>   0, 
        FC_RDMA_INITIATOR                                        =>   1, 
        FC_RDMA_INITIATOR_NO_TARGET_NOTIFICATION                 =>   2,
        FC_RDMA_INITIATOR_MULTICAST                              =>   3,
        FC_RDMA_INITIATOR_MULTICAST_NO_TARGET_NOTIFICATION       =>   4,
        FC_RDMA_INITIATOR_SG_MULTICAST                           =>   5,
        FC_RDMA_INITIATOR_SG_MULTICAST_NO_TARGET_NOTIFICATION    =>   6,
        FC_IP_INITIATOR                                          =>   7,
        FC_TARGET                                                => 256, 
        FC_IP_TARGET                                             => 257, 
        FC_RDMA_TARGET                                           => 258,
        FC_IP_TARGET_BROADCAST                                   => 259
      );

   -- ------------------------------------------------------------------ --


   Type FC_TRANSFER_STATUS_TYPE is
      ( FTS_SUCCESS,
        FTS_ERROR,
        FTS_ABORTED,
        FTS_INVALID_PORT,
        FTS_TIMEOUT,
        FTS_FCRM_INTERNAL_ERROR,
        FTS_TARGET_BUSY,       
        FTS_UNDERRUN,
        FTS_OVERRUN,
        FTS_CHECK_CONDITION,   
        FTS_TARGET_QUEUE_FULL,
        FTS_PCI_ERROR,
        FTS_WR_PROT_ERR,
        FTS_RD_PROT_ERR,
        FTS_BOUNDARY_ERR,
        FTS_E_D_TOV,
        FTS_ACK_TOV,
        FTS_BB_CREDIT_E_D_TOV,
        FTS_SEQ_CNT,
        FTS_RJT_RCVD,
        FTS_BSY_RCVD,
        FTS_ABTS_RCVD,
        FTS_LOSS_SYNC,
        FTS_AL_TIME,
        FTS_LP_TOV,
        FTS_LP_BB_CREDIT_TOV,
        FTS_CSU_RCVD,
        FTS_CSR_RCVD,
        FTS_TARGET_RESET,
        FTS_INVALID_REQUEST,
        FTS_BUS_RESET,
        FTS_INVALID_TARGET_TRANSFER, 
        FTS_DATA_PCI_ERROR,
        FTS_LUN_RESET,
        FTS_INITIATOR_UNDERRUN,
        FTS_SEGMENTED_TARGET_TRANSFER,
        FTS_PEND_SEGMENTED_TARGET_TRANSFER,
        FTS_PEND_TARGET_TRANSFER,    
        FTS_COMPLETE_TRANSEFR_DEFERRED_STATUS,
        FTS_COMPLETE_PENDED_TRANSFER,
        FTS_CHECK_PENDED_TRANSFER,   
        FTS_BUSY_PENDED_TRANSFER,    

        FTS_SPECIAL_STATUS
      );

   For FC_TRANSFER_STATUS_TYPE'size use 32;

   For FC_TRANSFER_STATUS_TYPE use
      ( FTS_SUCCESS                           =>   0,
        FTS_ERROR                             =>   1,
        FTS_ABORTED                           =>   2,
        FTS_INVALID_PORT                      =>   3,
        FTS_TIMEOUT                           =>   4,
        FTS_FCRM_INTERNAL_ERROR               =>   5,
        FTS_TARGET_BUSY                       =>   6,       
        FTS_UNDERRUN                          =>   7,
        FTS_OVERRUN                           =>   8,
        FTS_CHECK_CONDITION                   =>   9,   
        FTS_TARGET_QUEUE_FULL                 =>  10,
        FTS_PCI_ERROR                         =>  11,
        FTS_WR_PROT_ERR                       =>  12,
        FTS_RD_PROT_ERR                       =>  13,
        FTS_BOUNDARY_ERR                      =>  14,
        FTS_E_D_TOV                           =>  15,
        FTS_ACK_TOV                           =>  16,
        FTS_BB_CREDIT_E_D_TOV                 =>  17,
        FTS_SEQ_CNT                           =>  18,
        FTS_RJT_RCVD                          =>  19,
        FTS_BSY_RCVD                          =>  20,
        FTS_ABTS_RCVD                         =>  21,
        FTS_LOSS_SYNC                         =>  22,
        FTS_AL_TIME                           =>  23,
        FTS_LP_TOV                            =>  24,
        FTS_LP_BB_CREDIT_TOV                  =>  25,
        FTS_CSU_RCVD                          =>  26,
        FTS_CSR_RCVD                          =>  27,
        FTS_TARGET_RESET                      =>  28,
        FTS_INVALID_REQUEST                   =>  29,
        FTS_BUS_RESET                         =>  30,
        FTS_INVALID_TARGET_TRANSFER           =>  31,
        FTS_DATA_PCI_ERROR                    =>  32,
        FTS_LUN_RESET                         =>  33,
        FTS_INITIATOR_UNDERRUN                =>  34,
        FTS_SEGMENTED_TARGET_TRANSFER         =>  35,
        FTS_PEND_SEGMENTED_TARGET_TRANSFER    =>  36,
        FTS_PEND_TARGET_TRANSFER              =>  37,
        FTS_COMPLETE_TRANSEFR_DEFERRED_STATUS =>  38,
        FTS_COMPLETE_PENDED_TRANSFER          =>  39,
        FTS_CHECK_PENDED_TRANSFER             =>  40,
        FTS_BUSY_PENDED_TRANSFER              =>  41,
        FTS_SPECIAL_STATUS                    => 128
      );

   -- ------------------------------------------------------------------ --

   Type FCPResponseInfoType is array (1..FCP_RESPONSE_INFO_SIZE) of U8;
   Type SCSISenseDataType   is array (1..SCSI_SENSE_DATA_SIZE)   of U8;

   Type SCSIStatusInfoType is  
      Record
         SCSIStatus         : U16;
         Reserved1          : U16;
         Reserved2          : U16;
         Reserved3          : U16;
         ResponseInfoLength : U16;
         SenseDataLength    : U16;
         ResidualLength     : U32;
         FCPResponseInfo    : FCPResponseInfoType;
         SCSISenseData      : SCSISenseDataType;
      End Record;

   Type SCSIStatusInfoAccess is access SCSIStatusInfoType;      
         
   -- ------------------------------------------------------------------ --

   Type TransferInfoType;
   Type TransferInfoAccess is access TransferInfoType;                      

   Type Transfer_Callback_Proc is access 
      Procedure (TransferInfo : in TransferInfoAccess);

   Type SCSICommandBlockType is 
      array (1..SCSI_Command_Length) of System_Common_Types.Byte;

   For SCSICommandBlockType'size use 128; 


   Type TransferInfoType is
      Record
         HostID                : U8;
         TransferProtocol      : ProtocolType;
         Direction             : FC_DIRECTION_TYPE;
         Subaddress            : U16;
         
         -- Changed from U8 to U16.  New Firmware/API.  G. Prete 5-10-06
         Portxx                : U16;   
         
         Status                : FC_TRANSFER_STATUS_TYPE;
         BufferAddress         : System.Address;
         BufferSize            : U32;
         RemainingTransferSize : U32;
         RDMAOffset            : U32;
         PendedTargetHandle    : U32;
         DescriptorCount       : U16;
         Timeout               : U16;
         DescriptorPtr         : System.Address;
         TransferID            : U32;
         CDB                   : SCSICommandBlockType;
         Priority              : U8;
         SCSIError             : SCSIStatusInfoAccess;
         NotificationMethod    : Transfer_Callback_Proc;           
      End Record;


   For TransferInfoType use
      Record
         HostID                at  0 range 0..7;
         TransferProtocol      at  4 range 0..31;
         Direction             at  8 range 0..31;
         Subaddress            at 12 range 0..15;
         Portxx                at 14 range 0..15;
         Status                at 16 range 0..31;
         BufferAddress         at 20 range 0..31;
         BufferSize            at 24 range 0..31;
         RemainingTransferSize at 28 range 0..31;
         RDMAOffset            at 32 range 0..31;
         PendedTargetHandle    at 36 range 0..31;
         DescriptorCount       at 40 range 0..15;
         Timeout               at 42 range 0..15;
         DescriptorPtr         at 44 range 0..31;
         TransferID            at 48 range 0..31;
         CDB                   at 52 range 0..127;
         Priority              at 68 range 0..7;
         SCSIError             at 72 range 0..31;
         NotificationMethod    at 76 range 0..31;
      End Record;
       
   
   Null_Transfer_Info : constant TransferInfoType :=                                  
      ( HostID                => 0,
        TransferProtocol      => FC_INITIATOR,
        Direction             => FC_SEND,
        Subaddress            => 0,
        Portxx                => 0,
        Status                => FTS_SUCCESS,
        BufferAddress         => System.Null_Address,
        BufferSize            => 0,
        RemainingTransferSize => 0,
        RDMAOffset            => 0,
        PendedTargetHandle    => 0,
        DescriptorCount       => 0,
        Timeout               => 0,
        DescriptorPtr         => System.Null_Address,
        TransferID            => 0,
        CDB                   => (others=>0),
        Priority              => 0,
        SCSIError             => null,
        NotificationMethod    => null );
             
             
   -- ------------------------------------------------------------------ --

   Type FC_FARP_Direction_Type is
      ( FC_FARP_REQUESTOR,
        FC_FARP_RESPONDER
      );

   For FC_FARP_Direction_Type'size use 32;


   Type FC_FARP_Event_Type is
      ( FC_FARP_REQ_TX,
        FC_FARP_REQ_MATCH_RCVD, 
        FC_FARP_REPLY_TX, 
        FC_FARP_REPLY_RX_RCVD
      );

   For FC_FARP_Event_Type'size use 32;     
        
                 
   Type FC_Match_Code_Type is
      ( NO_MATCH,
        MATCH_PORT,
        MATCH_NODE,
        MATCH_BOTH
      );

   For FC_Match_Code_Type'size use 32;


   Type FC_Action_Code_Type is
      ( NO_ACTION,
        SND_LOGIN,
        SND_REPLY,
        LOGIN_AND_REPLY
      );

   For FC_Action_Code_Type'size use 32;


   Type FC_FARP_Status_Type is
      ( FC_SUCCESS,
        FC_NO_RSRC,
        FC_INVALID_MATCH_CODE,
        FC_INVALID_PORT, 
        FC_REJ_RCVD,
        FC_FARP_ERROR
      );

   For FC_FARP_Status_Type'size use 32;
            

   Type FARPInfo_Type;
   Type FARPInfoAccess is access FARPInfo_Type;                      

   Type FARP_Callback_Proc is access 
      Procedure (FARPInfo : in FARPInfoAccess);

   Type IPAddressType is array(1..16) of U8; 
   For IPAddressType'size use 128;
       
   Type FARPInfo_Type is
      Record    
         HostID              : U8;
         FARPId              : U32;
         FARPDirection       : FC_FARP_Direction_Type;
         FARPEvent           : FC_FARP_Event_Type;
         MatchCode           : FC_Match_Code_Type;
         ActionCode          : FC_Action_Code_Type;
         PortName            : WorldWideNameType;
         NodeName            : WorldWideNameType; 
         ReqPortID           : U32;
         IPAddress           : IPAddressType;
         Status              : FC_FARP_Status_Type;
         NotificationMethod  : FARP_Callback_Proc;
      End Record;

                              
   -- ------------------------------------------------------------------ --
                              
                              
   Type OmniPortManualModes is
      ( OmniVoid,
        AllPortLoopback,
        BypassQlogic,
        BypassPort1,
        BypassPort2,
        BypassPort1and2,
        BypassPort1andQlogic,
        BypassPort2andQlogic,
        AllPortsActive,
        RedundantChPort1,
        RedundantChPort2,
        RedundantLpPort1_2,
        RedundantLpPort1,
        RedundantLpPort2_1,
        RedundantLpPort2,
        RedundantLpNoQlogicPort1,
        RedundantLpNoQlogicPort2
      );

   For OmniPortManualModes'size use 32;                   

              
   Type Fibre_Device_Type is
      ( FD_ISP_2100,	
        FD_ISP_2200,
        FD_ISP_2300,
        FD_ISP_2310,
        FD_ISP_2312,
        FD_ISP_2322,
        FD_ISP_2422,
        FD_ISP_2432
      );
      
   For Fibre_Device_Type'size use 32;   
              

   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
   --                                                                    --
   --                   Public API Function Prototypes                   --
   --                                                                    --
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --

   -- FIBRE_INIT_ERR Fibre_Initialization
   --      ( BaseAddressType BASE_ADDRESS,
   --        u8  HOST_ID,
   --        u8  *WORKING_BUFFER,
   --  /*      u32 WORKING_BUFFER_PA,             */
   --  /*      u32 Firmware_Buffer_PA,            */
   --  /*      u32 RxSeq_Firmware_Buffer_PA,      */
   --  /*      u32 TxSeq_Firmware_Buffer_PA,      */
   --  /*      u8 *Firmware_Buffer,               */
   --  /*      u8 *RxSeqFirmwareBuffer,           */
   --  /*      u8 *TxSeqFirmwareBuffer,           */
   --  /*      u16 FIRMWARE_LENGTH,               */
   --  /*      u32 RxSeqFirmwareLen,              */
   --  /*      u32 TxSeqFirmwareLen,              */
   --        u16 FRAME_SIZE,
   --        u32 Options,
   --        u8  TimeOut,
   --        WorldWideNameType *WorldWideNameIn,
   --        WorldWideNameType *NodeWideNameIn,
   --        FIBRE_DEVICE_TYPE Device,
   --        void (*StatusHandler) (u8 Adapter, u32 Status, u32 Info)
   --      );
   --
   Type Initialization_Callback_Proc is access 
      Procedure (Adapter : in U8;  Status : in FC_STATUS_TYPE;  Info : in U32);
   
   Type Callback_Function is access Procedure (Status : in U32; ID : in U32);
       
   Function Fibre_Initialization
      ( Base_Address             : in BaseAddressType;
        Host_ID                  : in U8;
        Working_Buffer           : in System.Address;
--
--  Removed.  New Firmware/API.  G. Prete 5-10-06
--
--        Working_Buffer_PA        : in System.Address;
--        Firmware_Buffer_PA       : in System.Address;
--        RxSeq_Firmware_Buffer_PA : in System.Address;
--        TxSeq_Firmware_Buffer_PA : in System.Address;
--        Firmware_Buffer          : in System.Address;
--        RxSeqFirmwareBuffer      : in System.Address; 
--        TxSeqFirmwareBuffer      : in System.Address;
--        Firmware_Length          : in U16;
--        RxSeqFirmwareLen         : in U32; 
--        TxSeqFirmwareLen         : in U32;
        Frame_Size               : in U16;
        Options                  : in U32;
        Timeout                  : in U8;
        WorldWideNameIn          : in System.Address;
        NodeWideNameIn           : in System.Address;
        Device                   : in Fibre_Device_Type;
        StatusHandler            : in Initialization_Callback_Proc
      ) 
   return FIBRE_INIT_ERR;
   

   -- FIBRE_STATUS_TYPE Fibre_Status(u8 HOST_ID);
   --
   -- NOTE: Please note the syntax of the procedure header for the Ada
   --       version of this function.  It is NOT declared as an Ada function
   --       even though the C code is a function.  This is necessary
   --       due to the way the Ada and C compilers work when passing back
   --       record structures (structs) from a function.  If the C "struct"
   --       is 8 bytes or less, the C compiler will return the structure
   --       in two registers, which will not match the way the Ada compiler
   --       returns a similar Record structure from an Ada function.
   --
   Procedure Fibre_Status ( Status  : out FIBRE_STATUS_TYPE;
                            Host_ID : in U8 );
                                           

   -- FIBRE_TRANSFER_ERR Fibre_Transfer_Buffer
   --    ( TransferInfoType *TRANSFER_INFO );
   -- 
   Function Fibre_Transfer_Buffer
     ( TransferInfo : in TransferInfoAccess ) return FIBRE_TRANSFER_ERR; 
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Register_Target_Handler
   --    ( u8 HOST_ID, void (*TARGET_HANDLER)(TransferInfoType *) );
   -- 
   Function Fibre_Register_Target_Handler
      ( Host_ID        : in U8;
        Target_Handler : in Transfer_Callback_Proc ) return FIBRE_TRANSFER_ERR; 


   -- FIBRE_TRANSFER_ERR Fibre_Register_FARP_Handler
   --    (u8 HostID, void (*FARPHandler)(FARPInfoType *));
   --
   Function Fibre_Register_FARP_Handler
      ( Host_ID      : in U8;
        FARP_Handler : in FARP_Callback_Proc ) return FIBRE_TRANSFER_ERR; 

   
   -- FIBRE_TEST_ERR Fibre_Self_Test
   --    ( BaseAddressType BaseAddress, 
   --      u16             DeviceID, 
   --      Boolean         SkipRAMTest,
   --      u8              *RAMTestBuffer, 
   --      u32             RAMTestBufferPA, 
   --      u32             RAMSize, 
   --      u32             *FirstFailAddress );
   --
   Function Fibre_Self_Test
      ( BaseAddress      : in BaseAddressType;
        DeviceID         : in U16;
        SkipRAMTest      : in Integer;
        RAMTestBuffer    : in System.Address;
        RAMTestBufferPA  : in System.Address;
        RAMSize          : in U32;
        FirstFailAddress : in System.Address
      ) return FIBRE_TEST_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Service(u8 HOST_ID, Boolean *MyInt);
   -- 
   Function Fibre_Service
      ( Host_ID : in U8;
        MyInt   : in System.Address ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Abort_Target_Port
   --    ( u8  HostID, 
   --      u16 PortID,
   --      u32 ID, 
   --      void (*CallBackFunction)(u32 Status, u32 ID));
   --
   Function Fibre_Abort_Target_Port
      ( Host_ID  : in U8;
        PortID   : in U16;
        ID       : in U32;
        Callback : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Bus_Reset
   --    (u8 HostID,
   --     u32 ID, 
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   --                 
   Function Fibre_Bus_Reset
      ( Host_ID  : in U8;
        ID       : in U32;
        Callback : in Callback_Function ) return FIBRE_TRANSFER_ERR;
                    

   -- FIBRE_TRANSFER_ERR Fibre_Abort_Initiator_Transfer
   --    (u8 HostID, 
   --     FC_DIRECTION_TYPE Direction,
   --     u32 TransferID,
   --     u32 ID, 
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   --
   Function Fibre_Abort_Initiator_Transfer
      ( Host_ID    : in U8;
        Direction  : in FC_DIRECTION_TYPE;
        TransferID : in U32;
        ID         : in U32;
        Callback   : in Callback_Function ) return FIBRE_TRANSFER_ERR;


   -- FIBRE_TRANSFER_ERR Fibre_LUN_Reset
   --    (u8  HostID, 
   --     u16 PortID, 
   --     u16 LUN,
   --     u32 ID, 
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   --
   Function Fibre_LUN_Reset
      ( Host_ID    : in U8;
        PortID     : in U16;
        LUN        : in U16;
        ID         : in U32;
        Callback   : in Callback_Function ) return FIBRE_TRANSFER_ERR;

                    
   -- FIBRE_TRANSFER_ERR Fibre_Close(u8 HOST_ID, Boolean FastClose);
   -- 
   Function Fibre_Close 
      ( Host_ID   : in U8;
        FastClose : in integer ) return FIBRE_TRANSFER_ERR; 
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Abort_Target_Transfer
   --    (u8 HostID, FC_DIRECTION_TYPE Direction, u32 Transfer_ID);
   -- 
   Function Fibre_Abort_Target_Transfer
      ( Host_ID     : in U8;
        Direction   : in FC_DIRECTION_TYPE;
        Transfer_ID : in U32 ) return FIBRE_TRANSFER_ERR; 
   
   
   -- WorldWideNameType Fibre_Get_World_Wide_Name(BaseAddressType BaseAddress);
   -- 
   Function Fibre_Get_World_Wide_Name 
      ( BaseAddress : in BaseAddressType ) return WorldWideNameType;


   -- FIBRE_TRANSFER_ERR Fibre_Get_WWN_For_LoopID
   --    (u8  HostID, 
   --     u16 LoopID, 
   --     WorldWideNameType *WWN,
   --     Boolean GetNodeName,
   --     u32 ID, void (*CallBackFunction)(u32 Status, u32 ID));
   --
   Function Fibre_Get_WWN_For_LoopID
      ( Host_ID     : in U8;
        LoopID      : in U16;
        WWN         : in System.Address;
        GetNodeName : in integer;
        ID          : in U32;
        Callback    : in Callback_Function ) return FIBRE_TRANSFER_ERR;

   
   -- FIBRE_TRANSFER_ERR Fibre_Force_LIP
   --    (u8 HostID, u32 ID, void (*CallBackFunction)(u32 Status, u32 ID));
   -- 
   Function Fibre_Force_LIP
      ( Host_ID  : in U8;
        ID       : in U32;
        Callback : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_OmniPort_Control
   --    (u8 HostID, u16 Command, OmniPortManualModes Mode,
   --     u32 ID, void (*CallBackFunction)(u32 Status, u32 ID));
   --
   Function Fibre_OmniPort_Control
      ( Host_ID  : in U8;
        Command  : in U16;
        Mode     : in OmniPortManualModes;
        ID       : in U32;
        Callback : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Fabric_Alias_DID_To_LoopID
   --    (u8 HostID, u32 PortID24, u16 FabricTargetPortLoopIDAlias,
   --     u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID));
   --
   Function Fibre_Fabric_Alias_DID_To_LoopID
      ( Host_ID                     : in U8;
        PortID24                    : in U32;
        FabricTargetPortLoopIDAlias : in U16;
        CommandID                   : in U32;
        Callback                    : in Callback_Function ) 
   return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Fabric_Map
   --    (u8  HostID, 
   --     u32 Protocol, 
   --     u32 ResponseBufferPA, 
   --     u32 ResponseBufferLength,
   --     u32 CommandID, 
   --     void (*CallBackFunction)( u32 Status, u32 CommandID));
   --
   Function Fibre_Fabric_Map
      ( Host_ID                : in U8;
        Protocol               : in U32;
        ResponseBufferPA       : in U32;
        ResponseBufferLength   : in U32;
        CommandID              : in U32;
        Callback               : in Callback_Function ) 
   return FIBRE_TRANSFER_ERR;

     
   -- FIBRE_TRANSFER_ERR Fibre_Fabric_Get_All_PortIDs
   --    (u8  HostID, 
   --     u32 PortType, 
   --     u32 ResponseBufferPA, 
   --     u32 ResponseBufferLength,
   --     u32 CommandID, 
   --     void (*CallBackFunction)( u32 Status, u32 CommandID));
   --
   Function Fibre_Fabric_Get_All_PortIDs
      ( Host_ID                : in U8;
        PortType               : in U32;
        ResponseBufferPA       : in U32;
        ResponseBufferLength   : in U32;
        CommandID              : in U32;
        Callback               : in Callback_Function ) 
   return FIBRE_TRANSFER_ERR;

 
   -- FIBRE_TRANSFER_ERR Fibre_Fabric_Get_Next_PortID
   --    (u8  HostID, 
   --     u32 LastPortID,
   --     u32 ResponseBufferPA,
   --     u32 ResponseBufferLength,
   --     u32 CommandID, 
   --     void (*CallBackFunction)( u32 Status, u32 CommandID));
   --
   Function Fibre_Fabric_Get_Next_PortID
      ( Host_ID                : in U8;
        LastPortID             : in U32;
        ResponseBufferPA       : in U32;
        ResponseBufferLength   : in U32;
        CommandID              : in U32;
        Callback               : in Callback_Function ) 
   return FIBRE_TRANSFER_ERR;
   
                                    
   -- FIBRE_TRANSFER_ERR Fibre_Fabric_Register_Protocol
   --    (u8  HostID, 
   --     u32 PortID, 
   --     u32 Protocol, 
   --     u32 ResponseBufferPA, 
   --     u32 ResponseBufferLength,
   --     u32 CommandID, 
   --     void (*CallBackFunction)( u32 Status, u32 CommandID));
   --
   Function Fibre_Fabric_Register_Protocol
      ( Host_ID                : in U8;
        PortID                 : in U32;
        Protocol               : in U32;
        ResponseBufferPA       : in U32;
        ResponseBufferLength   : in U32;
        CommandID              : in U32;
        Callback               : in Callback_Function ) 
   return FIBRE_TRANSFER_ERR;
                         
                         
   -- FIBRE_TRANSFER_ERR Fibre_Register_RDMA
   --    (u8 HostID, u32 BufferBasePA, u32 ID,
   --     void (*NotificationHandler)(TransferInfoType *),
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   --                                                                                          
   Function Fibre_Register_RDMA
      ( Host_ID             : in U8;
        BufferBasePA        : in System.Address;
        ID                  : in U32;
        NotificationHandler : in Transfer_Callback_Proc;           
        Callback            : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- /* Added for enhanced RDMA */
   --
   -- FIBRE_TRANSFER_ERR Fibre_Register_RDMA_SubAddr
   --    (u8 HostID, u32 BufferBasePA, u32 length,
   --     u8 flags, u8 subaddress, u32 ID,
   --     void (*NotificationHandler)(TransferInfoType *),
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   -- 
   Function Fibre_Register_RDMA_SubAddr
      ( Host_ID             : in U8;
        BufferBasePA        : in System.Address;
        Length              : in integer;
        Flags               : in U8;
        Subaddress          : in U8;
        ID                  : in U32;
        NotificationHandler : in Transfer_Callback_Proc;           
        Callback            : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Register_RDMA_Multiple
   --    (u8 HostID, u8 *TablePtr, u32 TablePA, u32 ID,
   --     void (*NotificationHandler)(TransferInfoType *),
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   -- 
   Function Fibre_Register_RDMA_Multiple
      ( Host_ID             : in U8;
        TablePtr            : in System.Address;
        TablePA             : in U32;
        ID                  : in U32;
        NotificationHandler : in Transfer_Callback_Proc;           
        Callback            : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Get_SubAddr_List
   --    (u8 HOST_ID, u32 *ListPtr, u32 ListAddress, u32 CommandID, 
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   -- 
   Function Fibre_Get_SubAddr_List
      ( Host_ID     : in U8;
        ListPtr     : in System.Address;
        ListAddress : in U32;
        CommandID   : in U32;
        Callback    : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Get_SubAddr_Single
   --    (u8 HOST_ID, u8 SubAddress, u32 CommandID, 
   --     void (*CallBackFunction) (u16 Status, u32 Id, u16 Flgs_LenMSB,
   --                               u16 LenLSW, u16 AddrLSW, u16 AddrMSW));
   -- /* end enhanced RDMA addition */
   -- 
   Type Callback_Function_B is access 
      Procedure ( Status      : in U16; ID      : in U32;
                  Flgs_LenMSB : in U16; LenLSW  : in U16;
                  AddrLSW     : in U16; AddrMSW : in U16 );
   
   Function Fibre_Get_SubAddr_Single
      ( Host_ID    : in U8;
        Subaddress : in U8;
        CommandID  : in U32;
        Callback   : in Callback_Function_B ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Register_Fast_IP_Buffer
   --    (u8 HostID, u32 BufferBasePA, u16 NumberOfBlocks, u32 ID,
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   -- 
   Function Fibre_Register_Fast_IP_Buffer
      ( Host_ID        : in U8;
        BufferBasePA   : in U32;
        NumberOfBlocks : in U16;
        ID             : in U32;
        Callback       : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_LoopBack_Test
   --    (u8 HostID, u32 RXBufferBasePA, u32 TXBufferBasePA,
   --     u32 IterationCount, Boolean External, u32 Size, u32 ID,
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   --
   Function Fibre_LoopBack_Test
      ( Host_ID        : in U8;
        RXBufferBasePA : in U32;
        TXBufferBasePA : in U32;
        IterationCount : in U32;
        External       : in Integer;
        Size           : in U32;
        ID             : in U32;
        Callback       : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Enable_Watchdog
   --    (u8 HostID, u16 Timeout, u32 ID, 
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   -- 
   Function Fibre_Enable_Watchdog
      ( Host_ID  : in U8;
        Timeout  : in U16;
        ID       : in U32;
        Callback : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Enable_IP_Protocol
   --    (u8 HostID, u16 MTU, u16 IPHeaderSize,
   --     u16 RxBufferSize, u32 ID, 
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   --                                            
   Function Fibre_Enable_IP_Protocol
      ( Host_ID      : in U8;
        MTU          : in U16;
        IPHeaderSize : in U16;
        RxBufferSize : in U16;
        ID           : in U32;
        Callback     : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Define_Implicit_Port
   --    (u8 HostID, u8 LoopID, u32 RemotePortID, u16 RemoteFrameSize,
   --     u16 RemoteControlOptions, u32 ID,
   --     void (*CallBackFunction)(u32 Status, u32 ID));
   --                                               
   Function Fibre_Define_Implicit_Port
      ( Host_ID              : in U8;
        LoopID               : in U8;
        RemotePortID         : in U32;
        RemoteFrameSize      : in U16;
        RemoteControlOptions : in U16;
        ID                   : in U32;
        Callback             : in Callback_Function ) return FIBRE_TRANSFER_ERR;
   
   
   -- void Fibre_IntDisable(BaseAddressType BaseAddress);
   -- 
   Procedure Fibre_IntDisable (BaseAddress : in BaseAddressType);
   
   
   -- void Fibre_IntEnable(BaseAddressType BaseAddress);
   -- 
   Procedure Fibre_IntEnable (BaseAddress : in BaseAddressType);
   
   
   -- FIBRE_TRANSFER_ERR Fibre_Send_Mailbox_Command
   --    (u8 HostID, u16 MB0, u16 MB1, u16 MB2, u16 MB3, u16 MB6, u16 MB7,
   --    void (*CallBackFunction)(u16 MB0, u16 MB1, u16 MB2, 
   --                             u16 MB3, u16 MB6, u16 MB7));
   --
   Type Callback_Function_C is access 
      Procedure ( MB0 : in U16; MB1 : in U16;  MB2 : in U16;
                  MB3 : in U16; MB6 : in U16;  MB7 : in U16 );
   
   Function Fibre_Send_Mailbox_Command
      ( Host_ID   : in U8;
        MB0       : in U16;
        MB1       : in U16;
        MB2       : in U16;
        MB3       : in U16;
        MB6       : in U16;
        MB7       : in U16;
        Callback  : in Callback_Function_C ) return FIBRE_TRANSFER_ERR;
   


   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
   --                                                                    --
   --               C Procedure and Function Imports                     --
   --                                                                    --
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
   Pragma Import (C, Fibre_Initialization,          "Fibre_Initialization");
   Pragma Import (C, Fibre_Status,                  "Fibre_Status");
   Pragma Import (C, Fibre_Transfer_Buffer,         "Fibre_Transfer_Buffer");
   Pragma Import (C, Fibre_Register_Target_Handler,  
                    "Fibre_Register_Target_Handler");
   Pragma Import (C, Fibre_Register_FARP_Handler,  
                    "Fibre_Register_FARP_Handler");
   Pragma Import (C, Fibre_Self_Test,               "Fibre_Self_Test");
   Pragma Import (C, Fibre_Service,                 "Fibre_Service");
   Pragma Import (C, Fibre_Abort_Target_Port,       "Fibre_Abort_Target_Port");
   Pragma Import (C, Fibre_Bus_Reset,               "Fibre_Bus_Reset");
   Pragma Import (C, Fibre_Abort_Initiator_Transfer, 
                    "Fibre_Abort_Initiator_Transfer");
   Pragma Import (C, Fibre_LUN_Reset,               "Fibre_LUN_Reset");
   Pragma Import (C, Fibre_Close,                   "Fibre_Close");
   Pragma Import (C, Fibre_Abort_Target_Transfer,   
                    "Fibre_Abort_Target_Transfer");
   Pragma Import (C, Fibre_Get_World_Wide_Name,     
                    "Fibre_Get_World_Wide_Name");
   Pragma Import (C, Fibre_Get_WWN_For_LoopID,      "Fibre_Get_WWN_For_LoopID");
   Pragma Import (C, Fibre_Force_LIP,               "Fibre_Force_LIP");
   Pragma Import (C, Fibre_OmniPort_Control,        "Fibre_OmniPort_Control");
   Pragma Import (C, Fibre_Fabric_Alias_DID_To_LoopID,
                    "Fibre_Fabric_Alias_DID_To_LoopID");
   Pragma Import (C, Fibre_Fabric_Map,              "Fibre_Fabric_Map");
   Pragma Import (C, Fibre_Fabric_Get_All_PortIDs,
                    "Fibre_Fabric_Get_All_PortIDs");
   Pragma Import (C, Fibre_Fabric_Get_Next_PortID,
                    "Fibre_Fabric_Get_Next_PortID");
   Pragma Import (C, Fibre_Fabric_Register_Protocol,
                    "Fibre_Fabric_Register_Protocol");
   Pragma Import (C, Fibre_Register_RDMA,           "Fibre_Register_RDMA");
   Pragma Import (C, Fibre_Register_RDMA_SubAddr,
                    "Fibre_Register_RDMA_SubAddr");
   Pragma Import (C, Fibre_Register_RDMA_Multiple,   
                    "Fibre_Register_RDMA_Multiple");
   Pragma Import (C, Fibre_Get_SubAddr_List,        "Fibre_Get_SubAddr_List");
   Pragma Import (C, Fibre_Get_SubAddr_Single,      "Fibre_Get_SubAddr_Single");
   Pragma Import (C, Fibre_Register_Fast_IP_Buffer,
                    "Fibre_Register_Fast_IP_Buffer");
   Pragma Import (C, Fibre_LoopBack_Test,           "Fibre_LoopBack_Test");
   Pragma Import (C, Fibre_Enable_Watchdog,         "Fibre_Enable_Watchdog");
   Pragma Import (C, Fibre_Enable_IP_Protocol,      "Fibre_Enable_IP_Protocol");
   Pragma Import (C, Fibre_Define_Implicit_Port,
                    "Fibre_Define_Implicit_Port");
   Pragma Import (C, Fibre_IntDisable,              "Fibre_IntDisable");
   Pragma Import (C, Fibre_IntEnable,               "Fibre_IntEnable");
   Pragma Import (C, Fibre_Send_Mailbox_Command,    
                    "Fibre_Send_Mailbox_Command");
                     

   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --
   --                                                                    --
   --       Misc data types and functions for talking to PCI Bus         --
   --                                                                    --
   -- ------------------------------------------------------------------ --
   -- ------------------------------------------------------------------ --


   --
   --  Used in BSP procedure to search the PCI bus to find a specific device.
   --  This data type is defined in FC_PCIFunctions.c as well.
   --
   Type PCIConfigDataType is 
      Record
         PCIHose       : System.Address;      -- Pointer to PCI0 or PCI1 space.
         ConfigAddress : BaseAddressType;     -- Config base address of card.
         devNum        : integer;
         --
         --  IMPORTANT.  The padding here is necessary to make this record
         --  structure greater than 8 bytes!!!  It must be greater than 8 

         --  bytes so that the C function that returns it does NOT put the
         --  result into 2 registers.  This is all so we can call this
         --  thing from Ada and have the C and Ada compilers match up the
         --  register parameters.
         --
         Padding       : integer;  
      End Record;


   -- PCIConfigDataType GetPCIConfigAddress
   --    (int FUNC, UINT2 VENDOR, UINT2 DEVICE, int deviceNumber) 
   --
   -- NOTE: Please note the syntax of the procedure header for the Ada
   --       version of this function.  It is NOT declared as an Ada function
   --       even though the C code is a function.  This is necessary
   --       due to the way the Ada and C compilers work when passing back
   --       record structures (structs) from a function.  If the C "struct"
   --       is 8 bytes or less, the C compiler will return the structure
   --       in two registers, which will not match the way the Ada compiler
   --       returns a similar Record structure from an Ada function.
   --
   Procedure GetPCIConfigAddress
      ( ConfigData   : out PCIConfigDataType;
        Func         :  in integer;
        VendorID     :  in Interfaces.C.Unsigned_Short;
        DeviceID     :  in Interfaces.C.Unsigned_Short;
        DeviceNumber :  in integer
      );
   Pragma Import (C,GetPCIConfigAddress, "GetPCIConfigAddress"); 

                    
   -- 
   --  Function to completely disable one PCI Card.
   --  Called on OFP transitions.  This function resides in FC_PCIFunctions.C.
   --
   --  void Clear_PCI_Device
   --     ( BusSpace     PCIHose,
   --       unsigned int ConfigBaseAddress )
   --
   
   Procedure Clear_PCI_Device
      ( PCISpace          : in System.Address;
        ConfigBaseAddress : in FCAPI.BaseAddressType );
                    
   Pragma Import (C, Clear_PCI_Device, 
                    "Clear_PCI_Device");                 


   --
   --  Function to disable one 1553 chip.  This function resides in 
   --  FC_PCIFunctions.C
   --
   --
   --  void Disable_1553_Chip
   --     ( BusSpace     PCIHose,
   --       unsigned int ConfigBaseAddress )
   --
   Procedure Disable_1553_Chip
      ( PCISpace          : in System.Address;
        ConfigBaseAddress : in FCAPI.BaseAddressType );
                    
   Pragma Import (C, Disable_1553_Chip, 
                    "Disable_1553_Chip");            

                                                  
End FCAPI;

