/* ********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FCAPI.C    $Revision: 1.7 $
    Module Description:     User interface routines for the FCAPI

  Special Notes:


  Revision History
    Date        Description of Change
    ---------   ------------------------
    7/10/98     Version 1.0
    8/28/98     Version 2.0 See rel2_0.txt
    10/5/98     Version 2.1 See rel2_1.txt
    3/19/99     Version 2.2 See rel2_2.txt
    5/20/99     Version 2.3 See rel2_3.txt
    10/1/99     Version 3.0 See rel3_0.txt
    01/21/00    Version 3.11 See rel3_11.txt
    09/18/00    Version 3.2 See rel3_2.txt
    11/09/01    Version 4.0 See rel4_0.txt
    05/20/02    Version 4.1 See rel4_1.txt
    12/13/02    Version 4.5 See rel4_5.txt
    12/19/05    Version 6.0 See rel6_0.txt

  ************************************************************************
  *                                                                      *
  *                              NOTICE                                  *
  *                                                                      *
  *            COPYRIGHT 2000, 2001, 2002, 2003, 2004, 2005				 *
  *							 Critical IO, INC               			 *
  *                          ALL RIGHTS RESERVED                         *
  *                                                                      *
  * This computer program is CONFIDENTIAL  and contains TRADE SECRETS of *
  * CRITICAL IO, INC.  The  receipt or possession of this program        *
  * does not convey any rights to reproduce or disclose  its contents,   *
  * or to manufacture, use, or sell anything that it may describe, in    *
  * whole or in part, without the specific written consent of            *
  * CRITICAL IO, INC. Any reproduction of this program without the       *
  * express written consent of CRITICAL IO, INC is a violation of        *
  * the copyright laws and  may subject you to civil liability and       *
  * criminal prosecution.                                                *
  *                                                                      *
  ************************************************************************
******************************************************************************************** */

#include "fccommon.h"
#ifdef ISP2300
#include "fcmain2300hsdn.c"
#endif
#ifdef ISP2322
#include "fcmain2322hsdn.c"
//#include "fcmain2322hsdnsoftkey_byte.c"
#endif
#ifdef ISP24XX
#include "fcmain2432hsdn.c"
#endif

FIBRE_TEST_ERR FibreISP24XXSelfTest(BaseAddressType BaseAddress, u16 DeviceID, Boolean SkipRAMTest,
                                    u8 *RAMTestBuffer, u32 RAMTestBufferPA, u32 RAMSize, u32* FirstFailAddress);

/* *******************************************************************************************
    Fibre_Initialization

    Description:
        Initializes the FC-2100 host adapter

    Parameters:
        BaseAddressType BaseAddress:    Address of the Host adapter
        u8 HostID:                      The Fibre Channel Port ID to be assigned to the host adpater
        u8 *WorkingBuffer:              Address of a working buffer allocated to the FCAPI
        u32 Firmware_Buffer_PA          Physical Address of the firmware buffer if you want to download via DMA
        u32 WorkingBufferPA:            PCI bus physical address of the working buffer
        u16 *FIRMWARE_BUFFER:           Address of firmware to be loaded
        u16 FIRMWARE_LENGTH:            Number of words to load from the firmware buffer
        u8 NumSubadddresses             Number of LUNS (subaddresses) supported by the host adapter
        u16 FrameSize                   The number of bytes in the SCSI frames.
        u32 Options                     Flags for various options.
        u8 Timeout                      Transfer Timeout in seconds
        WorldWideNameType *WorldWideNameIn     Pointer to 8 byte world wide name 
                                               Normal use is FC_NULL - uses boards pre programmed WWN
                                               If pointer then user specifies WWN, used for embedded designs
        WorldWideNameType *NodeNameIn          Pointer to 8 byte world wide name (Node Name)
                                               Normal use is FC_NULL - uses boards pre programmed WWN as port name and node name
        void (*StatusHandler)(u8 Adapter, u32 Status, u32 Info)) This function allows the user to be informed of status
                                                                 change information from the NIC

    Return Value:
        FIBRE_INIT_ERR                  Initilization status

    Notes:
        Application must guarantee that only one instance of Fibre_Initialization is running at a time
******************************************************************************************* */

FIBRE_INIT_ERR Fibre_Initialization(BaseAddressType BaseAddress, u8 HostID, u8 *WorkingBuffer,
                                    u16 FrameSize, u32 Options,
                                    u8 TimeOut, WorldWideNameType *WorldWideNameIn,
                                    WorldWideNameType *NodeNameIn, FIBRE_DEVICE_TYPE Device,
                                    void (*StatusHandler)(u8 Adapter, u32 Status, u32 Info))
{
    static Boolean FirstTime = FC_TRUE;
#ifdef BIG_ENDIAN
	static EndianConverted = FC_FALSE;
#endif
    s16 i;
    volatile u32 Counter;
    u16 MB0;
    u16 InitStatus;
    u8  HostAdapterIndex;
    HostAdapterType *HostAdapter;
    u16 FPMHWRev;
    u32 WorkingBufferPA; 
    u32 Firmware_Buffer_PA, RxSeq_Firmware_Buffer_PA, TxSeq_Firmware_Buffer_PA;
    u8 *FirmwareBuffer, *RxSeqFirmwareBuffer, *TxSeqFirmwareBuffer;
    u16 FirmwareLength, RxSeqFirmwareLen, TxSeqFirmwareLen;

    if (HostID > PORT_ID_MAX)                               /* validate ID          */
       return FI_INVALID_HOST_ID;

    /* Frame Size must be within range and muliple of 8 */
    if (FrameSize < MIN_FRAME_SIZE || FrameSize > MAX_FRAME_SIZE || (FrameSize % 8 != 0))
        return FI_INVALID_FRAME_SIZE;

    if (FirstTime)                   /* if just started then initialize vars     */
    {
        FirstTime = FC_FALSE;
        for (i = 0; i < MAX_ADAPTERS; i++)
        {
            HostAdapters[i].BaseAddress             = 0;
            HostAdapters[i].Initialized             = FC_FALSE;
            HostAdapters[i].PortID                  = 0;
            HostAdapters[i].TargetHandler           = FC_NULL;            
            HostAdapters[i].MBCState                = NoMBCSent;
        }
    }

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++) /* Make sure not already initialized */
        if (HostAdapters[HostAdapterIndex].Initialized == FC_TRUE && HostAdapters[HostAdapterIndex].PortID == HostID)
            return FI_DEVICE_ALREADY_INITIALIZED;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++) /* find a slot to put it */
        if (HostAdapters[HostAdapterIndex].Initialized == FC_FALSE)
           break;

    if (HostAdapterIndex >= MAX_ADAPTERS)        /* make sure we can add a new adapter   */
        return (FI_TOO_MANY_ADAPTERS);

    HostAdapter = &HostAdapters[HostAdapterIndex];

    /* initialize host adapter structure */
    HostAdapter->BaseAddress        = BaseAddress;
    HostAdapter->PortID             = HostID;
    HostAdapter->Initialized        = FC_FALSE;                         /* keep False until we know it is initialized       */
    HostAdapter->IPInitialized      = FC_FALSE;
    if (WorldWideNameIn == FC_NULL)
        HostAdapter->WorldWideName  = GetWorldWideName(HostAdapter);    /* careful here--need BaseAddress and HostID set    */
    else
        HostAdapter->WorldWideName  = *WorldWideNameIn;

    if (NodeNameIn != FC_NULL)
    {
        HostAdapter->Flags.NodeNameEnabled  = FC_TRUE;
        HostAdapter->NodeName  = *NodeNameIn;
    }
/* Allocate space in working buffers in this order:
    Response Queue
    Request Queue
    Mailbox Command Buffer
    IP Receive Buffer Queue
*/
    WorkingBufferPA = FC_Processor_To_Physical(WorkingBuffer);
    HostAdapter->RespQPA            = WorkingBufferPA;
    HostAdapter->ReqQPA             = WorkingBufferPA + REQUEST_RESPONSE_QUEUE_SIZE;
    HostAdapter->MBCmdBufferPA      = WorkingBufferPA + (REQUEST_RESPONSE_QUEUE_SIZE * 2); /* place at end of working buffer */
    HostAdapter->RespQAddress       = WorkingBuffer;
    HostAdapter->ReqQAddress        = WorkingBuffer + (REQUEST_RESPONSE_QUEUE_SIZE/ADDRESSING_SIZE);
#ifdef IP_SUPPORT
    HostAdapter->IPRcvQAddressPA    = WorkingBufferPA + (REQUEST_RESPONSE_QUEUE_SIZE * 2) + MAILBOX_COMMAND_BUFFER_SIZE;
    HostAdapter->IPRcvQAddress      = WorkingBuffer + (((REQUEST_RESPONSE_QUEUE_SIZE * 2) + MAILBOX_COMMAND_BUFFER_SIZE)/ADDRESSING_SIZE);
    HostAdapter->IPRcvQHeadIndex    = 0;
    HostAdapter->IPRcvQTailIndex    = 0;
#endif
    /* Mailbox Command queue init */

    HostAdapter->MBCQueueBasePtr = Fibre_Malloc(sizeof(MBCQueueItemType) * MailboxCommandQueueSize);	
    HostAdapter->MBCQueueHead    = 0; 
    HostAdapter->MBCQueueTail    = 0;
    HostAdapter->MBCQueueNumEntries = 0;	            
    HostAdapter->MBCommandBuffer = WorkingBuffer + ((REQUEST_RESPONSE_QUEUE_SIZE * 2)/ADDRESSING_SIZE);
    HostAdapter->MBCommandBufferQ = HostAdapter->MBCommandBuffer + MAILBOX_COMMAND_BUFFER_BASE_SIZE; 
    HostAdapter->RespQHeadIndex     = 0;
    HostAdapter->RespQTailIndex     = 0;
    HostAdapter->ReqQHeadIndex      = 0;
    HostAdapter->ReqQTailIndex      = 0;
    HostAdapter->Waiting4ResetAck   = FC_FALSE;
    HostAdapter->TransferHandleCntr = 0;
    HostAdapter->TargetHandler      = FC_NULL;
    HostAdapter->RDMANotification   = FC_NULL;
    HostAdapter->StatusHandler      = (void *)StatusHandler;
    HostAdapter->FibreStatus.Valid                = FC_FALSE;
    HostAdapter->FibreStatus.FreeFCRMQueueEntries = 0;
    HostAdapter->FibreStatus.LoopUp               = FC_FALSE;
    HostAdapter->FibreStatus.LoopTransitionCount  = 0;          /* reset all counters                           */
    HostAdapter->FibreStatus.LIPCount             = 0;
    HostAdapter->FibreStatus.DroppedTransferCount = 0;
    for (i = 0; i < PORT_MAP_SIZE; i++)
        HostAdapter->FibreStatus.PortMap[i]       = FC_FALSE;   /* invalidate Port Map until we get new map     */
    HostAdapter->FibreStatus.PortMapValid         = FC_FALSE;
    HostAdapter->FibreStatus.IDValid              = FC_FALSE;
    HostAdapter->FibreStatus.ConnectionSpeed      = SPEED_1GIG;
    HostAdapter->FrameSize                        = FrameSize;
    HostAdapter->TimeOut                          = TimeOut;
    HostAdapter->OmniPortConfig                   = AUTO_HUB;
    HostAdapter->Flags.RequestLoopID              = FC_FALSE;
    HostAdapter->Flags.RequestPortMap             = FC_FALSE;
    HostAdapter->Flags.FastPostEnabled            = Options & FAST_POST_ENABLE_FLAG;  /* 3.1 legacy for fast post */
    HostAdapter->Flags.RequestLoopPortLogin       = FC_FALSE;
    HostAdapter->MBCState                         = NoMBCSent;
    HostAdapter->Options                          = Options;
    HostAdapter->SyncErr                          = FC_FALSE;
    HostAdapter->EntryCounter                     = 0;

    HostAdapter->AbortingTargetID                 = 255;
    HostAdapter->DevType						  = Device;
    init_transfer_queues(HostAdapter);                      /* initialize the queues used to hold transfer info */


#ifndef ISP24XX  /* if 24XX then do a different reset and device present test */

    FCWrite16(BaseAddress + IspCtlSts, 1);                  /* hardware reset                                   */
    FCDelay(FC2100ResetDelay);
    Counter = 0;
    while ((FCRead16(BaseAddress + IspCtlSts) & 0x01) && (Counter++ < 100000));
    if (Counter >= 100000)                                  /* Then never reset                             */
        return (FI_DEVICE_NOT_FOUND);

    FCWrite16(BaseAddress + Isp2PciIntCtl, 0);

    RESET_RISCfn(BaseAddress);        /* reset RISC                                   */

    FCDelay(FC2100ResetDelay);
    /* check for card present */

    FCWrite16(BaseAddress +IspFlshAdr, 0xA05F);             /* should be writable reg here always           */
    if (FCRead16(BaseAddress + IspFlshAdr) != 0xA05F)
        return (FB_DEVICE_NOT_FOUND);

    RELEASE_RISCfn(BaseAddress);          /* Allow boot firmware to run                   */

    FCDelay(FC2100ResetDelay);
    PAUSE_RISCfn(BaseAddress);        /* pause for access to risc registers           */

    Counter = 0;
    while (!(FCRead16(BaseAddress + IspHccr) & HCTLRPAUSED) && (Counter++ < 100000));/* wait for pause verification */
    if (Counter >= 100000)                                  /* Then never paused                            */
        return (FI_DEVICE_NOT_FOUND);

    FCWrite16(BaseAddress + IspCtlSts, FPM0_MODULE_SELECT);               /* select FPM regs                              */
    FPMHWRev = FCRead16(BaseAddress+IspFPMVer);             /* need to know if 2100, 2100A, 2200, or 2200A  */
    FCWrite16(HostAdapter->BaseAddress + IspFPMDiagCfg, ISP_FPM_RESET);   /* Reset FPM                                    */
    FCWrite16(HostAdapter->BaseAddress + IspFPMDiagCfg, 0);               /* Clear Reset FPM                              */

    FCWrite16(BaseAddress + IspCtlSts, 0);

    /* Program Counter always reset to 0x400        */
    /* and will be in range of 4XX                  */
    Counter = 0;
    while (((FCRead16(BaseAddress + IspPC) & 0xFF00) != 0x400)  && (Counter++ < 100000)); 
    if (Counter >= 100000) 
       return (FI_DEVICE_NOT_FOUND);                       

    FCWrite16(BaseAddress + IspAcc, 0xFA50);                /* make sure we can write to board              */
    if (FCRead16(BaseAddress + IspAcc) != 0xFA50)
        return (FI_DEVICE_NOT_FOUND);

    RELEASE_RISCfn(BaseAddress);          /* Allow boot firmware to run again             */


    /* Wait for RISC to be Ready */
    FCDelay(FC2100ResetDelay);

    Counter = 0;
    MB0 = FCRead16(BaseAddress + Mailbox0);
    while((MB0 != 0x0000)&&(Counter++ < 100000))
    {
        MB0 = FCRead16(BaseAddress + Mailbox0);
    }

#else /* ISP24XX */
    {
        u32 Counter;
        u16 MB0, MB3;
        
        FCWrite32(BaseAddress + IspCtlSts, 0x10000);            /* Shutdown any DMA activity */
        Counter = 0;
        while ((FCRead32(BaseAddress + IspCtlSts) & 0x20000) && (Counter++ < 100000));
        if (Counter >= 100000)                                  /* Then DMA never stopped    */
            return (FI_DEVICE_NOT_FOUND);
        
        FCWrite16(BaseAddress + IspCtlSts, 0x10001);            /* hardware reset  and DMA shut down */
        FCDelay(FC2100ResetDelay);
        Counter = 0;
        while ((FCRead16(BaseAddress + IspCtlSts) & 0x01) && (Counter++ < 100000));
        if (Counter >= 100000)                                  /* Then never reset          */
            return (FI_DEVICE_NOT_FOUND);
    
        FCDelay(20*FC2100ResetDelay);
    
        /* wait for reset completion and then verify chip there */       
    
        MB0 = FCRead16(BaseAddress + Mailbox0);
        while ((FCRead16(BaseAddress + Mailbox0) != 0) && (Counter++ < 100000));
        if (Counter >= 100000)                                  /* Then never reset          */
            return (FI_DEVICE_NOT_FOUND);
        if (FCRead16(BaseAddress + Mailbox1) != 0x4953)  /* should be "IS" */
            return (FI_DEVICE_NOT_FOUND);
        if (FCRead16(BaseAddress + Mailbox2) != 0x5020)  /* should be "P " */
            return (FI_DEVICE_NOT_FOUND);
        MB3 = FCRead16(BaseAddress + Mailbox3);
        if (Device == FD_ISP_2422)
        {
            if (MB3 != FC2422_DEVICE_ID) 
                return (FI_DEVICE_NOT_FOUND);
        }
        else
        {
            if (Device == FD_ISP_2432)
            {
                if (MB3 != FC2432_DEVICE_ID) 
                    return (FI_DEVICE_NOT_FOUND);
            }
            else /* invalid device selected */
                return (FI_DEVICE_NOT_FOUND);
        }
    }        
    
#endif /* ifndef ISP24XX */
    switch (Device)
	{
#ifdef ISP2300
		case FD_ISP_2300:
		case FD_ISP_2312:
		    FirmwareBuffer = FC_2300_risc_code;
		    RxSeqFirmwareBuffer = FC_NULL; 
			TxSeqFirmwareBuffer = FC_NULL;
		    Firmware_Buffer_PA = FC_Processor_To_Physical(FC_2300_risc_code);
			RxSeq_Firmware_Buffer_PA = FC_NULL;
			TxSeq_Firmware_Buffer_PA = FC_NULL;
			FirmwareLength = FC_2300_risc_code_length;
			RxSeqFirmwareLen = FC_NULL;
			TxSeqFirmwareLen = FC_NULL;
			break;
#endif
#ifdef ISP2322
		case FD_ISP_2322:
		    FirmwareBuffer = FC_2322_risc_code;
		    RxSeqFirmwareBuffer = FC_2322_rseq_code; 
			TxSeqFirmwareBuffer = FC_2322_xseq_code;
		    Firmware_Buffer_PA = FC_Processor_To_Physical(FC_2322_risc_code);
			RxSeq_Firmware_Buffer_PA = FC_Processor_To_Physical(FC_2322_rseq_code);
			TxSeq_Firmware_Buffer_PA = FC_Processor_To_Physical(FC_2322_xseq_code);
			FirmwareLength = FC_2322_risc_code_length;
			RxSeqFirmwareLen = FC_2322_rseq_code_length;
			TxSeqFirmwareLen = FC_2322_xseq_code_length;
			break;
#endif
#ifdef ISP24XX 
		case FD_ISP_2422:
		case FD_ISP_2432:
		    FirmwareBuffer = FC_2432_risc_code;
		    RxSeqFirmwareBuffer = FC_2432_rseq_code; 
			TxSeqFirmwareBuffer = FC_NULL;
		    Firmware_Buffer_PA = FC_Processor_To_Physical(FC_2432_risc_code);
			RxSeq_Firmware_Buffer_PA = FC_Processor_To_Physical(FC_2432_rseq_code);
			TxSeq_Firmware_Buffer_PA = FC_NULL;
			FirmwareLength = FC_2432_risc_code_length;
			RxSeqFirmwareLen = FC_2432_rseq_code_length;
			TxSeqFirmwareLen = FC_NULL;
			break;
#endif
		default:
			return FI_DEVICE_NOT_INITIALIZED;
	}

	if (Options & INIT_FW_WO_DMA_FLAG)
	{
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache(FirmwareBuffer, 2*FirmwareLength);
#endif
	    if (!DMARiscFirmware(Firmware_Buffer_PA, FirmwareLength, HostAdapter))
		    		return (FI_DMA_ERROR);

		if (Device >= FD_ISP_2322)
		{
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
	        Fibre_Flush_Cache(RxSeqFirmwareBuffer, 2*RxSeqFirmwareLen);
#endif
			LoadRiscRamExtended(RX_SEQ_FIRMWARE_START_ADDRESS, RxSeq_Firmware_Buffer_PA, RxSeqFirmwareLen, HostAdapter);
		}

		if (Device == FD_ISP_2322)
		{
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
		    Fibre_Flush_Cache((u8 *)TxSeqFirmwareBuffer, 2*TxSeqFirmwareLen);
#endif
			LoadRiscRamExtended(TX_SEQ_FIRMWARE_START_ADDRESS, TxSeq_Firmware_Buffer_PA, TxSeqFirmwareLen, HostAdapter);
		}
	}
    else
	{
	    if (LoadRiscFirmware((u16 *)FirmwareBuffer, FirmwareLength, HostAdapter) == FC_FALSE)
	    	return FI_DMA_ERROR;
		if (Device == FD_ISP_2322)
		{
			if (LoadRiscSeqFirmware((u16 *)RxSeqFirmwareBuffer, RxSeqFirmwareLen, RX_SEQ_FIRMWARE_START_ADDRESS, HostAdapter) == FC_FALSE)
				return FI_DMA_ERROR;
			if (LoadRiscSeqFirmware((u16 *)TxSeqFirmwareBuffer, TxSeqFirmwareLen, TX_SEQ_FIRMWARE_START_ADDRESS, HostAdapter) == FC_FALSE)
				return FI_DMA_ERROR;
		}
	}
    if (!VerifyRiscFwChecksum(HostAdapter))
        return (FI_CHECKSUM_FAIL);


    FCWrite16(BaseAddress+Mailbox4, 0);                         /* initialize mailbox queue ptr mailboxes */
    FCWrite16(BaseAddress+Mailbox5, 0);

	if (HostAdapter->DevType >= FD_ISP_2300)
	{
	    FCWrite16(BaseAddress+IspReqInPtr, 0);                      /* reset queue pointers         */
	    FCWrite16(BaseAddress+IspRespQOutPtr, 0);
	}

	#ifdef IP_SUPPORT
	if (FPMHWRev >= 3)                                              /* make sure a 2200  or better and if so reset IP Rcv Buffer Queue Ptr*/
	        FCWrite16(BaseAddress+Mailbox8, 0);
	#endif

    MB0 = ExecuteRiscFirmware(HostAdapter);
	if (MB0 != MB_STATUS_GOOD)
	{
		FCDEBUGPRINT(("Failed Execute RISC firmware - Status = %X !!!!!\n", MB0));
		if (MB0 == MB_STATUS_SEQ_CHKSUM_ERR)
			return FI_CHECKSUM_FAIL;
		else
			return FI_DEVICE_NOT_INITIALIZED;
	}
	if (HostAdapter->DevType >= FD_ISP_2312)
	{
		KeyType Key;
		Boolean Key_Sent = FC_FALSE;
		Key = Fibre_Read_Key(HostAdapter);
		#if 0
		{
			u8 i;
			for (i=0; i<8; i++)
				printf("B%d=%0X ", i, HostAdapter->WorldWideName.Bytes[i]);
			printf("Start sending softkey\n");
		}
		#endif	   
		if (!(Key_Sent = Send_Soft_Key(HostAdapter, Key)))
			return FI_KEY_FAIL;
	}

    InitStatus = InitializeFirmware(HostAdapter);               /* initialize and check status  */
    if (InitStatus != MB_STATUS_GOOD)
    {
FCDEBUGPRINT(("Initialization failed - error = %X\n", InitStatus));
        if (InitStatus == MB_STATUS_INVALID_KEY)
            return FI_KEY_FAIL;
        else
            return (FI_DEVICE_NOT_INITIALIZED);
    }

    HostAdapter->Initialized = FC_TRUE;

    if (InterruptMode)
        FCWrite16(BaseAddress + Isp2PciIntCtl, ENABLE_ALL_INTS | ENABLE_RISC_INT); /* Enable RISC Interrupt to PCI */
    return FI_SUCCESS;                                          /* everything  OK                           */

} /* end Fibre_Initialization */


/* *******************************************************************************************
    Fibre_Status

    Description:
        Returns the status of the FC-2100 Host Adapter and resource manager

    Parameters:
        u8 HOST_ID:                     Adapter ID

    Return Value:
        FIBRE_STATUS_TYPE:              Status structure

    Notes:

******************************************************************************************* */

FIBRE_STATUS_TYPE Fibre_Status(u8 HostID)
{

    FIBRE_STATUS_TYPE Status;
    HostAdapterType *HostAdapter;
    u16 HostAdapterIndex;

    Status.Valid = FC_FALSE;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                     */
        return Status;

    if (!Fibre_Lock(HostID)) return Status;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                        /* adapter not in list                  */
    {
        Fibre_Release(HostID);
        return Status;
    }

    HostAdapter = &HostAdapters[HostAdapterIndex];

    #if defined (CHECK_SYNCHRONIZATION)
        CheckSynchronization(HostAdapter);
    #endif

    Status = HostAdapter->FibreStatus;

    Status.Valid = FC_TRUE;

    Status.FreeFCRMQueueEntries = free_count(HostAdapter);

    HostAdapter->FibreStatus.LoopTransitionCount  = 0;   /* reset all counters                   */
    HostAdapter->FibreStatus.LIPCount             = 0;
    HostAdapter->FibreStatus.DroppedTransferCount = 0;

    #if defined (CHECK_SYNCHRONIZATION)
        ClearCheckSynchronization(HostAdapter);
    #endif

    Fibre_Release(HostID);

    return Status;

} /* end Fibre_Status */


/* *******************************************************************************************
    Fibre_Transfer_Buffer

    Description:
        Transmits data per the specified transfer protocol

    Parameters:
        TransferInfoType *TRANSFER_INFO (TI):   pointer to transfer information structure

    Return Value:
        FIBRE_TRANSFER_ERR:                     Status enumeration

    Notes:

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Transfer_Buffer(TransferInfoType *TI)
{
    u8 HostAdapterIndex;                                        /* location of Adapter assigned to HOST_ID  */
    HostAdapterType *HostAdapter;
    u32 TH;                                                     /* temp transfer handle                     */
#ifdef CHECK_DESCRIPTOR_LIST
    u32 i;
    u32 sum;
#endif
    FIBRE_TRANSFER_ERR TransferErr = FT_SUCCESS;

#ifndef DISABLE_ERROR_CHECKING
    if (TI->HostID > PORT_ID_MAX)                               /* validate host id                         */
        return FT_INVALID_HOST_ID;
#endif

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(TI->HostID))
        return FT_INVALID_HOST_ID;

    if ((TI->TransferProtocol >= FC_RDMA_INITIATOR_MULTICAST) &&
         (TI->TransferProtocol <= FC_RDMA_INITIATOR_SG_MULTICAST_NO_TARGET_NOTIFICATION) &&
        ((TI->DescriptorCount & 0xff00) == 0))
    {
      Fibre_Release(TI->HostID);
      return(FT_DESCRIPTOR_MISMATCH);
    }

    #if defined CHECK_DESCRIPTOR_LIST
    if (TI->DescriptorCount != 0)
    {
        sum = 0;
        for (i = 0; i < TI->DescriptorCount; i++)
        #ifdef SGL_64_BIT_SUPPORT
            sum += TI->DescriptorPtr[3*i + 2];              /* 64 bit addressing SGL */
        #else
            sum += TI->DescriptorPtr[2*i + 1];              /* 32 bit addressing SGL */
        #endif

        if (sum != TI->BufferSize)
        {
            Fibre_Release(TI->HostID);
            return(FT_DESCRIPTOR_MISMATCH);
        }
    }
    #endif

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == TI->HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list                  */
        TransferErr = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];              /* setup access variables               */

        #if defined (CHECK_SYNCHRONIZATION)
            CheckSynchronization(HostAdapter);
        #endif
        #ifdef ENABLE_AMCD
			if (!HostAdapter->FibreStatus.SyncValid)
			{
			    Fibre_Release(TI->HostID);
				return(FT_LOS_STATE);
			}
 		#endif 
        if (TI->Timeout == 0)
            TI->Timeout = HostAdapter->TimeOut;                     /* use default timeout if 0             */
#if 0
//        if (TI->Timeout & HIGH_RESOLUTION_TIMER)
//            TI->Timeout = ((((TI->Timeout-1) & ~HIGH_RESOLUTION_TIMER)/HIGH_RESOLUTION_TIMER_LSB)+2)
//                             | HIGH_RESOLUTION_TIMER;               /* +1tick/-0tick */
#endif
        if (TI->TransferProtocol == FC_SCSI_TARGET && TI->Status >= FTS_COMPLETE_PENDED_TRANSFER)
            TransferErr = Continue_Target_IO(TI, HostAdapter);
        else
        {
            TH = HostAdapter->TransferHandleCntr++ & XFER_CNTR_MASK;    /* set transfer handle                  */

            if (TI->Direction < FC_RECEIVE)                             /* check if send                        */
            {
                if (insert_q_item(HostAdapter,
                    (TH | SEND_TAG | (TI->TransferProtocol == FC_TARGET ? FC_ENTRY_AVAIL_MASK : 0)),
                    TI, FC_NULL) != FC_Q_SUCCESS)                       /* save info for later processing */
                    TransferErr = FT_INTERNAL_ERROR;
                else
                    if (TI->TransferProtocol < FC_IP_INITIATOR)         /* if SCSI or RDMA initiator then send command now    */
                        SendSCSICommand((TH | SEND_TAG), TI, HostAdapter);
#ifdef IP_SUPPORT
                    else if (TI->TransferProtocol == FC_IP_INITIATOR)
                        SendIPPacket((TH | SEND_TAG), TI, HostAdapter);
#endif
            }
            else
            {                                                              /* receive transfer                      */
                if (insert_q_item(HostAdapter, TH | (TI->TransferProtocol == FC_TARGET ? FC_ENTRY_AVAIL_MASK : 0),
                    TI, FC_NULL) != FC_Q_SUCCESS)                          /* save info for latter processing      */
                    TransferErr = FT_INTERNAL_ERROR;
                else
                    if (TI->TransferProtocol < FC_TARGET)                  /* if initiator then send command now    */
                        SendSCSICommand(TH, TI, HostAdapter);
            }
        } /* end not pended target transfer */
        #if defined (CHECK_SYNCHRONIZATION)
            ClearCheckSynchronization(HostAdapter);
        #endif
#ifdef CONT_IOCB_FOR_SG
		if (TI->Status == FTS_ERROR)
			TransferErr = FT_ADAPTER_QUEUE_FULL;
#endif
    } /* end else host adapter initialized */

    Fibre_Release(TI->HostID);

    /* Mutual exclusion ends here   */

    return TransferErr;

} /* end Fibre_Transfer_Buffer */


/* *******************************************************************************************
    Fibre_Register_Target_Handler

    Description:
        Registers a handler for target commands that have no buffers registers with the FCRM.

    Parameters:
        u8 HOST_ID:                     Adapter ID
        void (*TARGET_HANDLER)(NotificationType *)): Callback function to handle unregistered Target commands

    Return Value:
        FIBRE_TRANSFER_ERR:             Error enumeration

    Notes:

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Register_Target_Handler(u8 HostID, void (*TargetHandler)(TransferInfoType *))
{
    u8 HostAdapterIndex;
    FIBRE_TRANSFER_ERR TransferErr = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                                     */
        return FT_INVALID_HOST_ID;

    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list  */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                       /* adapter not in list                                  */
        TransferErr = FT_DEVICE_NOT_INITIALIZED;
    else
        HostAdapters[HostAdapterIndex].TargetHandler = TargetHandler;   /* assign target handler                        */

    Fibre_Release(HostID);

    return TransferErr;
}


/* *******************************************************************************************
    Fibre_Self_Test

    Description:
        Performs a invasive selftest of the FC-2100 host adapter

    Parameters:
        BaseAddressType BASE_ADDRESS:   Address of the Host adapter
        u16 DeviceID:                   Device ID from PCI configuration space 0x2100, 0x2200, 0x2300, 0x2312
        Boolean SkipRAMTest:            If true, RAM test will be skipped - Faster execution
        u8 *RAMTestBuffer:              A 64k BYTE buffer used for ram testing.  Only required for 2300s and up
        u32 RAMTestBufferPA:            The PCI address of the start of the ram test buffer
        u32 RAMSize:                    Number of BYTES of RAM on the card to test
        u32 *FirstFailAddress:          A pointer to a u32.  This will contain the address of the first ram failure if one occurs,
                                        otherwise it will be set to 0
                                        If SkipRAMTest is True or RAMSize is 0, RAMTestBuffer, RAMTestBufferPA and FirstFailAddress
                                        are not used and only 128K words are tested on a 2300

    Return Value:
        FIBRE_TEST_ERR:                 Status bit map

    Notes:
        Application is not allowed to call Fibre_Initialization for the same adapter until self test completes
******************************************************************************************* */

FIBRE_TEST_ERR Fibre_Self_Test(BaseAddressType BaseAddress, u16 DeviceID, Boolean SkipRAMTest,
                               u8 *RAMTestBuffer, u32 RAMTestBufferPA, u32 RAMSize, u32* FirstFailAddress)
{
    u16 Value, ReadValue;                                       /* Ram Test data                            */
    u16 Mailbox;
    u32 i;
    u16 RamLength, RamAddress;
    u16 Checksum;
    volatile u32 Counter;
    u16 FPMHWRev;
	HostAdapterType *HostAdapter;

    BaseAddressType HostAdapterAddress = BaseAddress;
    u8 HostAdapterIndex;


#ifdef ISP24XX
return FibreISP24XXSelfTest(BaseAddress, DeviceID, SkipRAMTest, *RAMTestBuffer, RAMTestBufferPA, RAMSize, FirstFailAddress);
#else


FCDEBUGPRINT(("Self test \n"));
    RAMSize = RAMSize/2;   /* convert to 16 bit words */

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++) /* Make sure not already initialized */
        if (HostAdapters[HostAdapterIndex].Initialized == FC_TRUE && HostAdapters[HostAdapterIndex].BaseAddress == BaseAddress)
            return FB_DEVICE_NOT_CLOSED;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++) /* find a slot to put it */
        if (HostAdapters[HostAdapterIndex].Initialized == FC_FALSE)
           break;

    if (HostAdapterIndex >= MAX_ADAPTERS)        /* make sure we can add a new adapter   */
        return (FI_TOO_MANY_ADAPTERS);

    HostAdapter = &HostAdapters[HostAdapterIndex];
	HostAdapter->BaseAddress = BaseAddress;
	HostAdapter->StatusHandler = FC_NULL;
	if (DeviceID < FC2300_DEVICE_ID)
		HostAdapter->DevType = FD_ISP_2200;
	else
		HostAdapter->DevType = FD_ISP_2300;
    FCWrite16(HostAdapterAddress + IspCtlSts, 1);               /* hardware reset                           */
    FCDelay(FC2100ResetDelay);
    Counter = 0;
    while ((FCRead16(HostAdapterAddress + IspCtlSts) & 0x01) && (Counter++ < 100000));

    if (Counter >= 100000)                                      /* Then never reset                         */
        return (FB_DEVICE_NOT_FOUND);

    FCWrite16(HostAdapterAddress + Isp2PciIntCtl, 0);

    RESET_RISCfn(HostAdapterAddress);     /* reset RISC                               */

    FCDelay(FC2100ResetDelay);
    RELEASE_RISCfn(HostAdapterAddress);       /* Allow boot firmware to run               */

    /* check for card present */
    FCDelay(FC2100ResetDelay);
    FCWrite16(HostAdapterAddress +IspFlshAdr, 0xA05F);          /* should be writable reg here always       */
FCDEBUGPRINT(("Flash \n"));
    if (FCRead16(HostAdapterAddress + IspFlshAdr) != 0xA05F)
        return (FB_DEVICE_NOT_FOUND);

    PAUSE_RISCfn(HostAdapterAddress);     /* pause for access to risc registers       */
FCDEBUGPRINT(("Pause \n"));

    Counter = 0;
    while (!(FCRead16(HostAdapterAddress + IspHccr) & HCTLRPAUSED) && (Counter++ < 100000));/* wait for pause verification */

    if (Counter >= 100000)                                      /* Then never paused                        */
        return (FB_DEVICE_NOT_FOUND);

FCDEBUGPRINT(("PC \n"));
    Counter = 0;
    while (((FCRead16(BaseAddress + IspPC) & 0xFF00) != 0x400)  && (Counter++ < 100000));
    if (Counter >= 100000)
        return (FB_DEVICE_NOT_FOUND);                          /* and will be in range of 4XX              */

    FCWrite16(HostAdapterAddress + IspAcc, 0xFA50);             /* make sure we can write to board          */
FCDEBUGPRINT(("FA50 \n"));
    if (FCRead16(HostAdapterAddress + IspAcc) != 0xFA50)
        return (FB_DEVICE_NOT_FOUND);

    FCWrite16(HostAdapterAddress + IspCtlSts, 0x20);            /* select FPM regs                          */
    FPMHWRev = FCRead16(HostAdapterAddress+IspFPMVer);          /* need to know if 2100, 2100A, 2200, or 2200A */

    FCWrite16(HostAdapterAddress + IspCtlSts, 0);

    RELEASE_RISCfn(HostAdapterAddress);       /* Allow boot firmware to run again         */

    /* Mailbox Test */

    WaitForMboxReady(HostAdapterAddress);
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_MB_REG_TEST);  /* write patterns to inbound mailboxes      */
    FCWrite16(HostAdapterAddress + Mailbox1, 0x0000);
    FCWrite16(HostAdapterAddress + Mailbox2, 0x5555);
    FCWrite16(HostAdapterAddress + Mailbox3, 0xAAAA);
    FCWrite16(HostAdapterAddress + Mailbox4, 0xFFFF);
    FCWrite16(HostAdapterAddress + Mailbox5, 0xFA50);
    FCWrite16(HostAdapterAddress + Mailbox6, 0xA50F);
    FCWrite16(HostAdapterAddress + Mailbox7, 0xF05A);

    SET_HOST2RISC_INTR(HostAdapterAddress);  /* tell adapter to loop data to outbound mailboxes   */
    Mailbox = WaitForMboxCmdCmpltn(HostAdapter);
    if(Mailbox != MB_STATUS_GOOD)
        return (FB_MAILBOX_FAIL);
    else
    {
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox1)) != 0x0000)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox2)) != 0x5555)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox3)) != 0xAAAA)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox4)) != 0xFFFF)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox5)) != 0xFA50)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox6)) != 0xA50F)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox7)) != 0xF05A)
            return (FB_MAILBOX_FAIL);
    }
    if (!SkipRAMTest)
    {
        if (DeviceID < FC2300_DEVICE_ID || RAMSize == 0) /* if ramsize = 0 then only do basic RAM test on 2300 */
        {
            if (FPMHWRev <= 2) /* old 2100 */
            {
                /* RISC RAM Test */

                Value = 0;                                          /* test 0 fill pattern      */

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)
                    RISC_Access(i, RISC_WRITE, &Value, HostAdapter);

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)
                {
                    RISC_Access(i, RISC_READ, &Value, HostAdapter);
                    if (Value != 0)
                        return (FB_RAM_FAIL);
                }

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)    /* test counting pattern    */
                {
                    Value = i;
                    RISC_Access(i, RISC_WRITE, &Value, HostAdapter);
                }

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)
                {
                    RISC_Access(i, RISC_READ, &Value, HostAdapter);
                    if (Value != i)
                        return (FB_RAM_FAIL);
                }
            }
            else /* 2100A or 2200 */
            {
                /* Speed improved RISC RAM Test -- takes 10's of msec compared to seconds       */
                /* we use the Boot loader's Init ram and checksum routines to speed up the test */


                Value      = 0x5555;                                                  /* test 5555 fill pattern                      */
                RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                RamAddress = RISC_RAM_START;

                while (RamLength > INIT_RAM_LENGTH)                                  /* can only set INIT_RAM_LENGTH words at a time*/
                {
                    InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                    RamAddress += INIT_RAM_LENGTH;
                    RamLength  -= INIT_RAM_LENGTH;
                }
                InitRiscRam(RamAddress, RamLength, Value, HostAdapter);  /* fake out checksum routine to think real code     */
                Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;          /* is present                                       */
                Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                Checksum = 0 - Checksum;
                Value = RISC_RAM_SIZE-RISC_RAM_START;
                RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);

                if (!VerifyRiscFwChecksum(HostAdapter))
                   return (FB_RAM_FAIL);

                Value      = 0xAAAA;                                                  /* test AAAA fill pattern                      */
                RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                RamAddress = RISC_RAM_START;

                while (RamLength > INIT_RAM_LENGTH)
                {
                    InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                    RamAddress += INIT_RAM_LENGTH;
                    RamLength  -= INIT_RAM_LENGTH;
                }
                InitRiscRam(RamAddress, RamLength, Value, HostAdapter);
                Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;
                Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                Checksum = 0 - Checksum;
                Value = RISC_RAM_SIZE-RISC_RAM_START;
                RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);
                if (!VerifyRiscFwChecksum(HostAdapter))
                   return (FB_RAM_FAIL);

FCDEBUGPRINT(("Completed basic RAM test\n"));
                /* now test for addressing errors                                                       */
                /* effectively write a value to each address bit and then make sure no overwrites occur */

                RamAddress = RISC_RAM_START;
                Value = 0;
                RISC_Access(RamAddress, RISC_WRITE, &Value, HostAdapter);
                Value = 1;
                for (i = 0; i < 16; i++)
                {
                    RISC_Access(RamAddress+Value, RISC_WRITE, &Value, HostAdapter);
                    Value = Value << 1;
                }
                RISC_Access(RamAddress, RISC_READ, &ReadValue, HostAdapter);
                if (ReadValue != 0)
                    return (FB_RAM_FAIL);
                Value = 1;
                for (i = 0; i < 16; i++)
                {
                    RISC_Access(RamAddress+Value, RISC_READ, &ReadValue, HostAdapter);
                    if (ReadValue != Value)
                        return (FB_RAM_FAIL);
                    Value = Value << 1;
                }
                if (FPMHWRev >= 0x06) /* then ISP2200A - we ship all 2200As with two RAMs */
                {
                    /* now test second bank of RAM*/

                    PAUSE_RISCfn(HostAdapterAddress);     /* pause for access to risc registers       */
                    Counter = 0;
#ifndef ISP24XX
                    while (!(FCRead16(HostAdapterAddress + IspHccr) & HCTLRPAUSED) && (Counter++ < 100000));/* wait for pause verification */
#else
                    while (!(FCRead32(HostAdapterAddress + IspHccr) & HCTLRPAUSED) && (Counter++ < 100000));/* wait for pause verification */
#endif
                    FCWrite16(HostAdapterAddress + 0xB0, 0xF2);                 /* select RAM Bank 2                        */
                    RELEASE_RISCfn(HostAdapterAddress);       /* Allow boot firmware to run again         */
                    Value = 0;                                                  /* test 0 fill pattern                      */

                    Value      = 0x5555;                                                  /* test 5555 fill pattern                      */
                    RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                    RamAddress = RISC_RAM_START;

                    while (RamLength > INIT_RAM_LENGTH)                                  /* can only set INIT_RAM_LENGTH words at a time*/
                    {
                        InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                        RamAddress += INIT_RAM_LENGTH;
                        RamLength  -= INIT_RAM_LENGTH;
                    }
                    InitRiscRam(RamAddress, RamLength, Value, HostAdapter);  /* fake out checksum routine to think real code     */
                    Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;          /* is present                                       */
                    Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                    Checksum = 0 - Checksum;
                    Value = RISC_RAM_SIZE-RISC_RAM_START;
                    RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                    RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);
                    if (!VerifyRiscFwChecksum(HostAdapter))
                       return (FB_RAM_FAIL);

                    Value      = 0xAAAA;                                                  /* test AAAA fill pattern                      */
                    RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                    RamAddress = RISC_RAM_START;

                    while (RamLength > INIT_RAM_LENGTH)
                    {
                        InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                        RamAddress += INIT_RAM_LENGTH;
                        RamLength  -= INIT_RAM_LENGTH;
                    }
                    InitRiscRam(RamAddress, RamLength, Value, HostAdapter);
                    Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;
                    Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                    Checksum = 0 - Checksum;
                    Value = RISC_RAM_SIZE-RISC_RAM_START;
                    RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                    RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);
                    if (!VerifyRiscFwChecksum(HostAdapter))
                       return (FB_RAM_FAIL);


                    /* now test for addressing errors                                                       */
                    /* effectively write a value to each address bit and then make sure no overwrites occur */

                    RamAddress = RISC_RAM_START;
                    Value = 0;
                    RISC_Access(RamAddress, RISC_WRITE, &Value, HostAdapter);
                    Value = 1;
                    for (i = 0; i < 16; i++)
                    {
                        RISC_Access(RamAddress+Value, RISC_WRITE, &Value, HostAdapter);
                        Value = Value << 1;
                    }
                    RISC_Access(RamAddress, RISC_READ, &ReadValue, HostAdapter);
                    if (ReadValue != 0)
                        return (FB_RAM_FAIL);
                    Value = 1;
                    for (i = 0; i < 16; i++)
                    {
                        RISC_Access(RamAddress+Value, RISC_READ, &ReadValue, HostAdapter);
                        if (ReadValue != Value)
                            return (FB_RAM_FAIL);
                        Value = Value << 1;
                    }
                }
            }
        }
        else /* 2300 or higher device and RamSize > 0*/ /* this is a fast DMA based test and also tests more than first 128Kx16 */
        {
            u32 *Ptr = (u32*)RAMTestBuffer;
            u32 RAMAddress;
            u32 TestValue;
            u32 NumAddressLines;
            u32 Address;

            *FirstFailAddress = 0;

            TestValue = 0xAAAAAAAAL;
            for (i = 0; i < RISC_RAM_SIZE/2; i++) /* fill memory with first test pattern (32 bits) RISC_RAM_SIZE is 16 bit words */
                *Ptr++ = TestValue;
            RAMSize = RAMSize & 0xFFFF0000L;     /* make sure multiple of 64K words*/
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                LoadRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
            }
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                Ptr = (u32*)RAMTestBuffer;
                for (i = 0; i < RISC_RAM_SIZE/2; i++) /* clear memory  (32 bits) */
                    *Ptr++ = 0;
                DumpRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
                Ptr = (u32*)RAMTestBuffer + RISC_RAM_START/2;
                for (i = RISC_RAM_START; i < RISC_RAM_SIZE/2; i++) /* Test memory (32 bits at a time) */
                    if (*Ptr++ != TestValue)
                    {
                       *FirstFailAddress = RAMAddress+i;
                        return(FB_RAM_FAIL);
                    }
            }

            Ptr = (u32*)RAMTestBuffer;
            TestValue = 0x55555555L;
            for (i = 0; i < RISC_RAM_SIZE/2; i++) /* fill memory with first test pattern (32 bits) RISC_RAM_SIZE is 16 bit words */
                *Ptr++ = TestValue;
            RAMSize = RAMSize & 0xFFFF0000L;     /* make sure multiple of 64K words*/
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                LoadRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
            }
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                Ptr = (u32*)RAMTestBuffer;
                for (i = 0; i < RISC_RAM_SIZE/2; i++) /* clear memory  (32 bits) */
                    *Ptr++ = 0;
                DumpRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
                Ptr = (u32*)RAMTestBuffer + RISC_RAM_START/2;
                for (i = RISC_RAM_START; i < RISC_RAM_SIZE/2; i++) /* Test memory (32 bits at a time) */
                    if (*Ptr++ != TestValue)
                    {
                       *FirstFailAddress = RAMAddress+i;
                        return(FB_RAM_FAIL);
                    }
            }

            /* now test for addressing errors                                                       */
            /* effectively write a value to each address bit and then make sure no overwrites occur */

            for ( NumAddressLines = 0; RAMSize != 0; NumAddressLines++)  /* calculate the number of address lines going to memory */

            {
                RAMSize = RAMSize >> 1;
            }

            RamAddress = RISC_RAM_START;
            WriteRiscRamExtended(RamAddress, 0xFFFF, HostAdapter);
            Address = 1;
            for (i = 1; i < NumAddressLines; i++)
            {
                WriteRiscRamExtended(RamAddress+Address, i, HostAdapter);  /* write address line # into Memory Location */
                Address = Address << 1;
            }
            ReadValue = ReadRiscRamExtended(RamAddress, HostAdapter);
            if (ReadValue != 0xFFFF)
            {

                *FirstFailAddress = RamAddress;
                return (FB_RAM_FAIL);
            }
            Address = 1;
            for (i = 1; i < NumAddressLines; i++)
            {
                ReadValue = ReadRiscRamExtended(RamAddress+Address, HostAdapter);
                if (ReadValue != i)
                {
                    *FirstFailAddress = RamAddress+Address;
                     return (FB_RAM_FAIL);
                }
                Address = Address << 1;
            }
        }
    }
    return FB_SUCCESS;       /* got this far then everything worked  */
#endif /* ISP24XX */

} /* end Fibre_Self_Test */



/* *******************************************************************************************
    Fibre_Service

    Description:
        Services any requests from the FC-2100 host adapter

    Parameters:
        u8 HostID:          The Fibre Channel ID of the host adapter to service
        Boolean *MyInt:     Set to true if this we found the Board's interrupt active

    Return Value:
        FIBRE_TRANSFER_ERR  Error status

    Notes:

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Service(u8 HostID, Boolean *MyInt)
{
    u16 MB5;
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    Boolean IntActive;

    *MyInt = FC_FALSE;
#ifndef DISABLE_ERROR_CHECKING
    if (HostID > PORT_ID_MAX)                                   /* validate host id                 */
        return FT_INVALID_HOST_ID;
#endif

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                       /* adapter not in list              */
        return FT_DEVICE_NOT_INITIALIZED;

    HostAdapter = &HostAdapters[HostAdapterIndex];

    #if defined (CHECK_SYNCHRONIZATION)
        CheckSynchronization(HostAdapter);
    #endif

    IntActive = ServiceRiscInterrupt(HostAdapter);                          /* see if any interrupts active     */

    if (IntActive)
    {
		if (HostAdapter->DevType < FD_ISP_2300)
	    {
	        MB5 = FCRead16(HostAdapter->BaseAddress + Mailbox5);

	        while (HostAdapter->RespQTailIndex != MB5)
	        {
	            ProcessResponseQueue(HostAdapter);
	        }
		}
        CLEAR_RISC2HOST_INTR(HostAdapter->BaseAddress);    /* clear RISC interrupt to host */
    }

	if (HostAdapter->DevType >= FD_ISP_2300)
    {
    	u32	RespQCnt =0;
        MB5 = FCRead16(HostAdapter->BaseAddress + IspRespQInPtr);

        while (HostAdapter->RespQTailIndex != MB5)
        {
            ProcessResponseQueue(HostAdapter);
            MB5 = FCRead16(HostAdapter->BaseAddress + IspRespQInPtr);
			HostAdapter->RespQCnt++;
			RespQCnt++;
        }
	}


    #if defined (CHECK_SYNCHRONIZATION)
        ClearCheckSynchronization(HostAdapter);
    #endif

    *MyInt = IntActive;
    return FT_SUCCESS;

} /* end Fibre_Service */


/* *******************************************************************************************
    Fibre_Close

    Description:
        Closes all existing communicaitions with the specified host adapter.  Fibre_Initialize must
        be called prior to issuing commands to a closed host adapter.

    Parameters:
        u8 HostID:          The Fibre Channel ID of the host adapter to close
        Boolean FastClose   If true, the port will be closed even if outstanding transfers or commands
                            are in progress
    Return Value:
        FIBRE_TRANSFER_ERR  Error status

    Notes:

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Close(u8 HostID, Boolean FastClose)
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter = FC_NULL;
    volatile u32 Counter;
    FIBRE_TRANSFER_ERR TransferErr = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        TransferErr = FT_DEVICE_NOT_INITIALIZED;

    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (!FastClose)
        {
            if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
            {
                Fibre_Release(HostID);
                return FT_OUTSTANDING_REQUEST;
            }
            else
                if (clear_q(HostAdapter) != FC_Q_SUCCESS)               /* clear out all pending transfers  */
                    TransferErr = FT_INTERNAL_ERROR;
        }
        /* at this point reset the board anyway even if an error has been detected                          */

        FCWrite16(HostAdapter->BaseAddress + IspCtlSts, 1);             /* hardware reset                   */

        FCDelay(FC2100ResetDelay);

        Counter = 0;
        while ((FCRead16(HostAdapter->BaseAddress + IspCtlSts) & 0x01) && (Counter++ < 100000));
#ifndef ISP24XX

        PAUSE_RISCfn(HostAdapter->BaseAddress);
        Counter = 0;
        while (!(FCRead16(HostAdapter->BaseAddress + IspHccr) & PAUSE_RISC) && (Counter++ < 100000));
        FCWrite16(HostAdapter->BaseAddress + IspCtlSts, FPM0_MODULE_SELECT);  /* Select bank 0 FPM  */
        FCWrite16(HostAdapter->BaseAddress + IspFPMDiagCfg, ISP_FPM_RESET);  /* Reset FPM  */
        FCWrite16(HostAdapter->BaseAddress + IspFPMDiagCfg, 0);               /* Clear Reset FPM  */
        FCWrite16(HostAdapter->BaseAddress + IspCtlSts, 0);

        FCWrite16(HostAdapter->BaseAddress + Isp2PciIntCtl, 0);

        RESET_RISCfn(HostAdapter->BaseAddress);   /* reset RISC                       */

        FCDelay(FC2100ResetDelay);
        RELEASE_RISCfn(HostAdapter->BaseAddress);     /* Allow boot firmware to run       */

#endif
        Fibre_Free(HostAdapter->TransferRecord, NUM_TRANSFER_RECORDS * sizeof(transfer_record_type));
        Fibre_Free(HostAdapter->MBCQueueBasePtr, sizeof(MBCQueueItemType) * MailboxCommandQueueSize);

        HostAdapter->BaseAddress     = 0;
        HostAdapter->Initialized     = FC_FALSE;                        /* disable host adapter structure   */
        HostAdapter->PortID          = 0;
        HostAdapter->TargetHandler   = FC_NULL;            
        HostAdapter->MBCState        = NoMBCSent;
    }

    Fibre_Release(HostID);

    return TransferErr;

} /* end Fibre_Close */


/* *******************************************************************************************
    Fibre_Abort_Target_Transfer

    Description:
        Removes the transfer designated by TransferID from the designated FCRM queue.

    Parameters:
        u8 HostID:                     The Fibre Channel ID of adapter where the transfer was assigned
        FC_DIRECTION_TYPE Direction:   FC_SEND or FC_RECEIVE
        u32 TransferID:                User assigned ID for the transfer to be removed

    Return Value:
        FIBRE_TRANSFER_ERR  Error status

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Abort_Target_Transfer(u8 HostID, FC_DIRECTION_TYPE Direction, u32 TransferID)
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR TransferErr = FT_SUCCESS;
    FC_Q_ERR_TYPE q_status;


    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        TransferErr = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];

        q_status = remove_q_item(HostAdapter, Direction, TransferID);

        switch (q_status)
        {
        case FC_Q_SUCCESS:
            break;

        case FC_Q_NOT_FOUND:
            TransferErr = FT_UNKNOWN_RECORD;
            break;

        default:
            TransferErr = FT_INTERNAL_ERROR;
        }
    }

    Fibre_Release(HostID);

    return TransferErr;

}

/* *******************************************************************************************
    Fibre_Abort_Initiator_Transfer

    Description:
        Aborts the transfer designated by TransferID from the designated FCRM queue.
        The callback function will be called to indicate status of the abort command
        If successful, the original transfer notification method will be called indicating abort status
    Parameters:
        u8 HostID:                     The Fibre Channel ID of adapter where the transfer was assigned
        FC_DIRECTION_TYPE Direction:   FC_SEND or FC_RECEIVE
        u32 TransferID:                User assigned ID for the transfer to be removed
        u32 ID:                        User ID returned to command completion callback
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = invalid target ID or LUN
                    2 = invalid handle or loop down
                    3-255 - Reserved.

            ID - the 32 bit ID assigned to this command

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Abort_Initiator_Transfer(u8 HostID, FC_DIRECTION_TYPE Direction, u32 TransferID,
                                                  u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    u32 Handle;
    FIBRE_TRANSFER_ERR TransferErr = FT_SUCCESS;
    FC_Q_ERR_TYPE q_status;
    TransferInfoType TI;


    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        TransferErr = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];

        q_status = find_q_item_by_xferID(HostAdapter, Direction, FC_INITIATOR, TransferID, &Handle, &TI);
        switch (q_status)
        {
            case FC_Q_SUCCESS:
                HostAdapter = &HostAdapters[HostAdapterIndex];
                if (HostAdapter->MBCState != NoMBCSent)           
                {
                    MBCQueueItemType MBCQueueItem; 

                    MBCQueueItem.MBCState              = AbortInitiatorSent;				
                    MBCQueueItem.Callback              = CallBackFunction;;				
                    MBCQueueItem.ClientID              = ID;             
                    MBCQueueItem.Mailbox[0]            = MBC_ABORT_CMD_IOCB;			
                    if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                        MBCQueueItem.Mailbox[1]            = TI.Port;           
                    else
                    MBCQueueItem.Mailbox[1]            = TI.Port << 8;			
                    MBCQueueItem.Mailbox[2]            = Handle & 0xFFFF;			
                    MBCQueueItem.Mailbox[3]            = Handle >> 16;			
                    MBCQueueItem.Mailbox[6]            = TI.Subaddress;			
                    MBCQueueItem.NumMailboxRegs        = 5;       
            
                    if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                        TransferErr = FT_OUTSTANDING_REQUEST;
                    Fibre_Release(HostID);
                    return TransferErr;
                }
                HostAdapter->MBCState                   = AbortInitiatorSent;
                HostAdapter->MBCCallBackFunction        = CallBackFunction;
                HostAdapter->MBCClientID                = ID;
                FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_ABORT_CMD_IOCB );
                if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                    FCWrite16(HostAdapter->BaseAddress + Mailbox1, TI.Port);
                else
                FCWrite16(HostAdapter->BaseAddress + Mailbox1, TI.Port << 8);
                FCWrite16(HostAdapter->BaseAddress + Mailbox2, Handle & 0xFFFF);
                FCWrite16(HostAdapter->BaseAddress + Mailbox3, Handle >> 16);
                FCWrite16(HostAdapter->BaseAddress + Mailbox6, TI.Subaddress);
                SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

                break;

            case FC_Q_NOT_FOUND:
                TransferErr = FT_UNKNOWN_RECORD;
                break;

            default:
                TransferErr = FT_INTERNAL_ERROR;
        }
    }

    Fibre_Release(HostID);

    return TransferErr;

} /* Fibre_Abort_Initiator_Transfer */


/* *******************************************************************************************
    Fibre_Abort_Target_Port

    Description:
        Clears out all transfer commands to the specified Target port.
        All in progress transfers will have their callback functions called with abort status.

    Parameters:
        u8 HostID:      The host adapter ID
        u8 PortID:      The port to be aborted
        u32 ID:         User ID returned to command completion callback
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:        Status enumeration

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = invalid target ID or LUN
                    2 = loop down
                    3-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Abort_Target_Port(u8 HostID, u16 PortID,
                                           u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))

{
    u8 HostAdapterIndex;                                        /* location of Adapter assigned to HOST_ID  */
    HostAdapterType *HostAdapter;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                         */
        return FT_INVALID_HOST_ID;

    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                               /* adapter not in list                  */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];                  /* setup access variables               */

        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = PortAbortRequestSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_ABORT_TARGET;			
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[1]            = PortID;            
            else
            MBCQueueItem.Mailbox[1]            = PortID << 8;			
            MBCQueueItem.Mailbox[2]            = 0;			
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[10]            = 0;            
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.NumMailboxRegs        = 11;
            else
            MBCQueueItem.NumMailboxRegs        = 3;       
            MBCQueueItem.AbortingTargetID      = PortID;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState = PortAbortRequestSent;
        HostAdapter->MBCCallBackFunction  = CallBackFunction;
        HostAdapter->MBCClientID          = ID;
        HostAdapter->AbortingTargetID     = PortID;
        HostAdapter->ResetingLUN          = 0;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_ABORT_TARGET); /* send abort target                  */
        if (0)// (HostAdapter->DevType >= FD_ISP_2322)
            FCWrite16(HostAdapter->BaseAddress + Mailbox1, PortID);
        else
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, PortID << 8);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, 0);                /* delay = 0                          */
        FCWrite16(HostAdapter->BaseAddress + Mailbox10, 0);                
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;

} /* end Fibre_Abort_Port */


/* *******************************************************************************************
    Fibre_Bus_Reset

    Description:
        Resets all devices on the loop.
        The callback functions for any outstanding commands will be called with abort status.

    Parameters:
        u8 HostID:      The host adapter ID
        u32 ID:         User ID returned to command completion callback
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function
        
    Return Value:
        FIBRE_TRANSFER_ERR:        Status enumeration

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = error
                3-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Bus_Reset(u8 HostID,
                                   u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))

{
    u8 HostAdapterIndex;                                        /* location of Adapter assigned to HOST_ID  */
    HostAdapterType *HostAdapter;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                         */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                               /* adapter not in list                  */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];                  /* setup access variables               */

        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = BusResetSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_RESET;			
            MBCQueueItem.Mailbox[1]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 2;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState            = BusResetSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = ID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_RESET);          /* send abort target                  */
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, 0);                  /* delay = 0                          */
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;

} /* end Fibre_Bus_Reset */


/* *******************************************************************************************
    Fibre_LUN_Reset

    Description:
        Resets the specified Logical Unit Number

    Parameters:
        u8 HostID:      The host adapter ID
        u8 PortID:      The target port
        u16 LUN:        The Lun to be aborted
        u32 ID:         User ID returned to command completion callback
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:        Status enumeration

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = invalid target ID or LUN
                    2 = loop down
                    3-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_LUN_Reset(u8 HostID, u16 PortID, u16 LUN,
                                   u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))

{
    u8 HostAdapterIndex;                                        /* location of Adapter assigned to HOST_ID  */
    HostAdapterType *HostAdapter;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                         */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                               /* adapter not in list                  */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];                  /* setup access variables               */

        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = LUNResetSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_LUN_RESET;			
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[1]            = PortID;            
            else
            MBCQueueItem.Mailbox[1]            = PortID << 8;			
            MBCQueueItem.Mailbox[2]            = LUN;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 4;       
            MBCQueueItem.AbortingTargetID      = PortID;  	
            MBCQueueItem.ResetingLUN           = LUN;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState            = LUNResetSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = ID;
        HostAdapter->AbortingTargetID    = PortID;
        HostAdapter->ResetingLUN         = LUN;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_LUN_RESET);  /* Send LUN reset                */
        if (0)// (HostAdapter->DevType >= FD_ISP_2322)
            FCWrite16(HostAdapter->BaseAddress + Mailbox1, PortID);
        else
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, PortID << 8);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, LUN);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, 0);
                     
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;

} /* end Fibre_LUN_Reset */



/* *******************************************************************************************
    Fibre_Get_World_Wide_Name

    Description:
        Extracts the adapter's world wide name from the eeprom on the card

    Parameters:
        none

    Return Value:
        WorldWideNameType:       The structure contains the WWN

    Notes:  *** This function must be called prior to initialization ***

******************************************************************************************* */
WorldWideNameType Fibre_Get_World_Wide_Name(BaseAddressType BaseAddress)
{

    HostAdapterType HostAdapter;

    HostAdapter.BaseAddress = BaseAddress;

    return GetWorldWideName(&HostAdapter);
}


/* *******************************************************************************************
    Fibre_Get_WWN_For_LoopID

    Description:
        Returns the World Wide Name for the specified Loop ID

    Parameters:
        u8 HostID:  The Fibre Channel ID of adapter where the transfer was assigned
        u8 LoopID:  The Fibre Channel ID of node to return the WWN for
        WorldWideNameType *:  A persistent pointer to the WorldWideName structure to return the WWN to
                              Note that the WWN will not be filled in until the completion callback is invoked
        Boolean GetNodeName:  if true then the Node Name is returned, otherwise the Port Name is returned
        u32 ID   :  User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: The call back function will not be called be if error status.

        Callback status is 0 = success
                           1 = no link
                           2 = Loop ID not logged in                 

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Get_WWN_For_LoopID(u8 HostID, u16 LoopID, WorldWideNameType *WWN,  Boolean GetNodeName,
                                            u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* Try queueing command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetWWNSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_PORT_NAME;			
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[1]            = (LoopID << 8);         
            else
            MBCQueueItem.Mailbox[1]            = (LoopID << 8) | (GetNodeName ? 1:0);			
            MBCQueueItem.Mailbox[2]            = 0;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[10]           = (GetNodeName ? 1:0);           
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.NumMailboxRegs        = 11;       
            else
            MBCQueueItem.NumMailboxRegs        = 4;       
	        MBCQueueItem.WWNPtr                = WWN;
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = GetWWNSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        HostAdapter->WWNPtr                     = WWN;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_PORT_NAME);  /* Get Port/Node Name */
        if (0)// (HostAdapter->DevType >= FD_ISP_2322)
            FCWrite16(HostAdapter->BaseAddress + Mailbox1, LoopID << 8);        
        else
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, (LoopID << 8) | (GetNodeName ? 1:0));        
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, 0);
        if (0)// (HostAdapter->DevType >= FD_ISP_2322)
            FCWrite16(HostAdapter->BaseAddress + Mailbox10, (GetNodeName ? 1:0));
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */
    
    return Status;
}


/* *******************************************************************************************
    Fibre_Get_Name_Id_List

    Description:
        Returns the WWN, PortID, and LoopId for all logged in ports

    Parameters:
        u8 HostID:  The Fibre Channel ID of adapter where the transfer was assigned
        u8 Options:  Specify if want loop id's 0-125 (bit 1 ==0), or 0-255 (bit 1 ==1)
		u32 ListAddress: Specify the target address for the returned data structures
        u32 ID   :  User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: The call back function will not be called be if error status.

        Callback status is 0 = success
                           1 = no link
                           2 = Loop ID not logged in                 

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Get_Name_Id_List(u8 HostID, u8 Options, NameIdListType *List,
                                            u32 ID, void (*CallBackFunction)(u32 Status, u32 ID, u8 NumPorts))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetNameIdListSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_NAME_ID_LIST;			
            MBCQueueItem.Mailbox[1]            = Options;			
            MBCQueueItem.Mailbox[2]            = HostAdapter->MBCmdBufferPA >> 16;			
            MBCQueueItem.Mailbox[3]            = HostAdapter->MBCmdBufferPA;			
            MBCQueueItem.NumMailboxRegs        = 4;       
            MBCQueueItem.MBSNameIdListPointer  = List; 
    
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = GetNameIdListSent;
        HostAdapter->MBCCallBackFunction        = (void(*))CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        HostAdapter->MBSNameIdListPointer       = List;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_NAME_ID_LIST);  /* Get Port/Node Name */
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, Options);        
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */
    
    return Status;
}


/* *******************************************************************************************
    Fibre_Force_LIP

    Description:
        Forces a LIP to occur on the bus.  The CallBackFunction is called with Status upon completion.

    Parameters:
        u8 HostID:  The Fibre Channel ID of adapter where the transfer was assigned
        u32 ID   :  User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status.

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Force_LIP(u8 HostID, u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = ForceLIPSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_INITIATE_LIP;			
            MBCQueueItem.Mailbox[1]            = 0;			
            MBCQueueItem.Mailbox[2]            = 0;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 4;       
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = ForceLIPSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_INITIATE_LIP); /* Force LIP */
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}


/* *******************************************************************************************
    Fibre_OmniPort_Control

    Description:
        Allows for user control of the OmniPort HUB.
        Returns Port status to call back function.

    Parameters:
        u8 HostID   : The Fibre Channel ID of adapter where the transfer was assigned
        u16 Control : What function to perform
        OmniPortManualModes Mode    : The data associated with the command
        u32 ID      : User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.

                Bit 8 = Port 1 active (OK)
                Bit 9 = Port 2 active (OK)
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_OmniPort_Control(u8 HostID, u16 Command, OmniPortManualModes Mode,
                                         u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = OmniPortControlSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_OMINPORT_CONFIG;			
            MBCQueueItem.Mailbox[1]            = Command;			
            MBCQueueItem.Mailbox[2]            = Mode;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = OmniPortControlSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        HostAdapter->OmniPortConfig             = Command;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_OMINPORT_CONFIG);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, Command);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, Mode);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}


/* *******************************************************************************************
    Fibre_Fabric_Alias_DID_To_LoopID

    Description:

        Returns a 8 bit LoopID to alias the provided 24 bit Destination Port ID.  Attempts to assign the
        provided alias to the DID.  Returns the assigned alias - either the requested one if not
        already assinged, or the currently assigned alias, or 0xFFFF if an error.
        Loop ID alias should start from 0xFE and go down.  The LoopID is returned to the callback function

        If PORTID is set to 0xFFFFFFFF, then the loop id will be logged out.


    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u32 PortID24                     :   24 bit port ID of the target (DID) or 0xFFFFFFFF for logout
        u8  FabricTargetPortLoopIDAlias  :   8 bit suggested loop ID alias (0xFF downto 0x00)
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 AliasID, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status/AliasID :   assigned loop ID alias range 0x0 - 0xFF.
                                                Returns lower 16 bits=0xFFFF if error - Note this is different from others.
                                                If error, then the upper 16 bits is 
                                                assigned a value from FC_ALIAS_STATUS_TYPE defined in FCAPI.H
                        u32 CommandID       :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Fabric_Alias_DID_To_LoopID(u8 HostID, u32 PortID24, u16 FabricTargetPortLoopIDAlias,
                                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = IDAliasSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = CommandID;             
            if (PortID24 <= 0xFFFFFF)           
                MBCQueueItem.Mailbox[0]        = MBC_LOGIN_FABRIC_PORT;	
            else		
                MBCQueueItem.Mailbox[0]        = MBC_LOGOUT_FABRIC_PORT;	
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[1]            = FabricTargetPortLoopIDAlias;           
            else
            MBCQueueItem.Mailbox[1]            = FabricTargetPortLoopIDAlias << 8;			
            MBCQueueItem.Mailbox[2]            = PortID24 >> 16;			
            MBCQueueItem.Mailbox[3]            = PortID24 & 0xFFFF;			
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[10]            = 0;            
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.NumMailboxRegs        = 11;
            else
            MBCQueueItem.NumMailboxRegs        = 4;       
            MBCQueueItem.MBCAlias              = FabricTargetPortLoopIDAlias;
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = IDAliasSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;
        HostAdapter->FabricTargetPortLoopIDAlias = FabricTargetPortLoopIDAlias;
        if (PortID24 <= 0xFFFFFF) /* then alias (login) */
            FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_LOGIN_FABRIC_PORT); /* Assign alias ID */
        else
            FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_LOGOUT_FABRIC_PORT); /* DeAssign alias ID */

        if (0)// (HostAdapter->DevType >= FD_ISP_2322)
            FCWrite16(HostAdapter->BaseAddress + Mailbox1, FabricTargetPortLoopIDAlias);
        else
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, (FabricTargetPortLoopIDAlias << 8));
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, PortID24 >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, PortID24 & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox10, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}


/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Map_Fabric(u8 HostID, u32 Protocol, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Performs a SNS GNN_FT command to return all Port IDs and Node Names on the fabric that have registered
        as supporting the specified Fibre Channel Protocol.

        SCSI_FCP protocol code is 0x08.

    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u32 Protocol                     :   Fibre Channel Protocol code to request map on
        u32 ResponseBufferPA             :   PCI physical address of the buffer to put the response in
        u32 ResponseBufferLength         :   Size of response buffer in bytes
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 on success, 1 otherwise
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:
    The data that is returned in the response buffer consists of a 16 byte header followed
    by a list of entries.  Use the control byte to determine if you are at the last entry in the list.
    The following structures are defined in FCAPI.h

    typedef struct                          define Fabric controller SNS map fabric response
    {
        u8 Header[16];                     16 bit word at offset 8 is completion status, 8002h = success, 8001h = fail (Big Endian format)
    }SNSResponseHeaderType;

    typedef struct
    {
        u8 ControlByte;                      0 = entry valid, 0x80 = last entry
        u8 PortID[3];                        24 bit portID in Big Endian format
        u8 Reserved[4];
        u8 NodeName[8];                      Big endian format and upper nibble (type) is always 0
    }SNSResponseEntryType;

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Fabric_Map(u8 HostID, u32 Protocol, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        SNS_GNN_FT_Type *SNSCommand;

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 
            u32 CommandBufferQOffset = HostAdapter->MBCQueueHead*MAILBOX_COMMAND_BUFFER_QUEUE_SIZE;
            
            if (HostAdapter->MBCQueueNumEntries == MailboxCommandQueueSize) /* then queue full so quit here */
            {
                Fibre_Release(HostID);
                return FT_OUTSTANDING_REQUEST;
            }
             
            SNSCommand = (SNS_GNN_FT_Type *) HostAdapter->MBCommandBufferQ+CommandBufferQOffset;
                         /* need to allocate a different Command buffer for each command that uses buffer as input to command */

            SNSCommand->ResponseLength16    = ResponseBufferLength/2;
            SNSCommand->Reserved1           = 0;
            SNSCommand->ResponseBufPALow    = ResponseBufferPA;
            SNSCommand->ResponseBufPAHigh   = 0;
            SNSCommand->SubCommandLength    = SNS_GNN_FT_SUBCMD_LENGTH16;
            SNSCommand->Reserved2           = 0;
            SNSCommand->SubCommand          = SNS_GNN_FT_CMD;
            SNSCommand->ResponseLength32    = ResponseBufferLength/4;
            SNSCommand->Reserved3           = 0;
            SNSCommand->Protocol            = Protocol;
            			
#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_GNN_FT_Type)/4, 0, 0, FC_FALSE);
#endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_GNN_FT_Type));
#endif

            MBCQueueItem.MBCState              = SNSSent;				
            MBCQueueItem.Callback              = CallBackFunction;	
            MBCQueueItem.ClientID              = CommandID;
            MBCQueueItem.Mailbox[0]            = MBC_SEND_SNS;	
            MBCQueueItem.Mailbox[1]            = SNS_GNN_FT_CMD_LENGTH16;			
            MBCQueueItem.Mailbox[2]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset) >> 16;			
            MBCQueueItem.Mailbox[3]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset);			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;       
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = SNSSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;

        SNSCommand = (SNS_GNN_FT_Type *) HostAdapter->MBCommandBuffer;

        SNSCommand->ResponseLength16    = ResponseBufferLength/2;
        SNSCommand->Reserved1           = 0;
        SNSCommand->ResponseBufPALow    = ResponseBufferPA;
        SNSCommand->ResponseBufPAHigh   = 0;
        SNSCommand->SubCommandLength    = SNS_GNN_FT_SUBCMD_LENGTH16;
        SNSCommand->Reserved2           = 0;
        SNSCommand->SubCommand          = SNS_GNN_FT_CMD;
        SNSCommand->ResponseLength32    = ResponseBufferLength/4;
        SNSCommand->Reserved3           = 0;
        SNSCommand->Protocol            = Protocol;

#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_GNN_FT_Type)/4, 0, 0, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_GNN_FT_Type));
#endif

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SEND_SNS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, SNS_GNN_FT_CMD_LENGTH16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Fabric_Get_All_PortIDs(u8 HostID, u32 PortType, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Performs a SNS GID_PT command to return all Port IDs for the specific port type 
        To see all Nx Ports (N ports, NLPorts or any with port type less than 0x80) set PortType to 0x7F
    
    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u32 PortType                     :   Typically 0x7E for all Nx Ports
        u32 ResponseBufferPA             :   PCI physical address of the buffer to put the response in
        u32 ResponseBufferLength         :   Size of response buffer in bytes
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 on success, 1 otherwise
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:
    The data that is returned in the response buffer consists of a 16 byte header followed
    by a list of entries.  Use the control byte to determine if you are at the last entry in the list.
    The following structures are defined in FCAPI.h

    typedef struct                          define Fabric controller SNS map fabric response
    {
        u8 Header[16];                     16 bit word at offset 8 is completion status, 8002h = success, 8001h = fail (Big Endian format)
    }SNSResponseHeaderType;

    typedef struct
    {
        u8 ControlByte;                      0 = entry valid, 0x80 = last entry
        u8 PortID[3];                        24 bit portID in Big Endian format
    }SNSGetPortIDsType;

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Fabric_Get_All_PortIDs(u8 HostID, u32 PortType, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        SNS_GID_PT_Type *SNSCommand;

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 
            u32 CommandBufferQOffset = HostAdapter->MBCQueueHead*MAILBOX_COMMAND_BUFFER_QUEUE_SIZE;
            
            if (HostAdapter->MBCQueueNumEntries == MailboxCommandQueueSize) /* then queue full so quit here */
            {
                Fibre_Release(HostID);
                return FT_OUTSTANDING_REQUEST;
            }
             
            SNSCommand = (SNS_GID_PT_Type *) HostAdapter->MBCommandBufferQ+CommandBufferQOffset;
                         /* need to allocate a different Command buffer for each command that uses buffer as input to command */

            SNSCommand->ResponseLength16    = ResponseBufferLength/2;
            SNSCommand->Reserved1           = 0;
            SNSCommand->ResponseBufPALow    = ResponseBufferPA;
            SNSCommand->ResponseBufPAHigh   = 0;
            SNSCommand->SubCommandLength    = SNS_GID_PT_SUBCMD_LENGTH16;
            SNSCommand->Reserved2           = 0;
            SNSCommand->SubCommand          = SNS_GID_PT_CMD;
            SNSCommand->ResponseLength32    = ResponseBufferLength/4;
            SNSCommand->Reserved3           = 0;
            SNSCommand->PortType            = PortType;
            			
#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_GID_PT_Type)/4, 0, 0, FC_FALSE);
#endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_GID_PT_Type));
#endif

            MBCQueueItem.MBCState              = SNSSent;				
            MBCQueueItem.Callback              = CallBackFunction;	
            MBCQueueItem.ClientID              = CommandID;
            MBCQueueItem.Mailbox[0]            = MBC_SEND_SNS;	
            MBCQueueItem.Mailbox[1]            = SNS_GID_PT_CMD_LENGTH16;			
            MBCQueueItem.Mailbox[2]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset) >> 16;			
            MBCQueueItem.Mailbox[3]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset);			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;       
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = SNSSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;

        SNSCommand = (SNS_GID_PT_Type *) HostAdapter->MBCommandBuffer;

        SNSCommand->ResponseLength16    = ResponseBufferLength/2;
        SNSCommand->Reserved1           = 0;
        SNSCommand->ResponseBufPALow    = ResponseBufferPA;
        SNSCommand->ResponseBufPAHigh   = 0;
        SNSCommand->SubCommandLength    = SNS_GID_PT_SUBCMD_LENGTH16;
        SNSCommand->Reserved2           = 0;
        SNSCommand->SubCommand          = SNS_GID_PT_CMD;
        SNSCommand->ResponseLength32    = ResponseBufferLength/4;
        SNSCommand->Reserved3           = 0;
        SNSCommand->PortType            = PortType;

#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_GID_PT_Type)/4, 0, 0, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_GID_PT_Type));
#endif

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SEND_SNS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, SNS_GID_PT_CMD_LENGTH16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}




/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Fabric_Get_Next_PortID(u8 HostID, u32 PortID, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Performs a SNS GA_NXT command to return the next higher Port IDs Node Names on the fabric

    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u32 PortID                       :   24 bit port ID (last port ID).  The call will return the WWN for next higher Port ID
        u32 ResponseBufferPA             :   PCI physical address of the buffer to put the response in
        u32 ResponseBufferLength         :   Size of response buffer in bytes.  Should be allocated to 608 bytes
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 on success, 1 otherwise
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:
    The data that is returned in the response buffer consists of a 16 byte header followed by the data:

    typedef struct                          define Fabric controller SNS map fabric response
    {
        u8 Header[16];
    }SNSResponseHeaderType;

    typedef struct
    {
        u8 PortType;
        u8 PortID[3];                        24 bit portID in Big Endian format
        u8 PortName[8];                      Big endian format and upper nibble (type) is always 0
        u8 Reserved[608-38]                  other details not of specific interest.
    }SNSGetNextResponseType;

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Fabric_Get_Next_PortID(u8 HostID, u32 LastPortID, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        SNS_GNN_FT_Type *SNSCommand;

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 
            u32 CommandBufferQOffset = HostAdapter->MBCQueueHead*MAILBOX_COMMAND_BUFFER_QUEUE_SIZE;

            if (HostAdapter->MBCQueueNumEntries == MailboxCommandQueueSize) /* then queue full so quit here */
            {
                Fibre_Release(HostID);
                return FT_OUTSTANDING_REQUEST;
            }
            
            SNSCommand = (SNS_GNN_FT_Type *) HostAdapter->MBCommandBufferQ+CommandBufferQOffset;
                         /* need to allocate a different Command buffer for each command that uses buffer as input to command */

            SNSCommand->ResponseLength16    = ResponseBufferLength/2;
            SNSCommand->Reserved1           = 0;
            SNSCommand->ResponseBufPALow    = ResponseBufferPA;
            SNSCommand->ResponseBufPAHigh   = 0;
            SNSCommand->SubCommandLength    = SNS_GA_NXT_SUBCMD_LENGTH16;
            SNSCommand->Reserved2           = 0;
            SNSCommand->SubCommand          = SNS_GA_NXT_CMD;
            SNSCommand->ResponseLength32    = ResponseBufferLength/4;
            SNSCommand->Reserved3           = 0;
            SNSCommand->Protocol            = LastPortID;
            			
#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_GNN_FT_Type)/4, 0, 0, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_GNN_FT_Type));
#endif
            MBCQueueItem.MBCState              = NextSNSSent;				
            MBCQueueItem.Callback              = CallBackFunction;	
            MBCQueueItem.ClientID              = CommandID;
            MBCQueueItem.Mailbox[0]            = MBC_SEND_SNS;	
            MBCQueueItem.Mailbox[1]            = SNS_GA_NXT_CMD_LENGTH16;			
            MBCQueueItem.Mailbox[2]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset) >> 16;			
            MBCQueueItem.Mailbox[3]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset);			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;       
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = NextSNSSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;

        SNSCommand = (SNS_GNN_FT_Type *) HostAdapter->MBCommandBuffer;

        SNSCommand->ResponseLength16    = ResponseBufferLength/2;
        SNSCommand->Reserved1           = 0;
        SNSCommand->ResponseBufPALow    = ResponseBufferPA;
        SNSCommand->ResponseBufPAHigh   = 0;
        SNSCommand->SubCommandLength    = SNS_GA_NXT_SUBCMD_LENGTH16;
        SNSCommand->Reserved2           = 0;
        SNSCommand->SubCommand          = SNS_GA_NXT_CMD;
        SNSCommand->ResponseLength32    = ResponseBufferLength/4;
        SNSCommand->Reserved3           = 0;
        SNSCommand->Protocol            = LastPortID;

#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_GNN_FT_Type)/4, 0, 0, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_GNN_FT_Type));
#endif

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SEND_SNS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, SNS_GA_NXT_CMD_LENGTH16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}


/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Fabric_Register_Protocol(u8 HostID, u32 PortID, u32 Protocol, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Registers the specified protocol with the Fabric.  Needed for other nodes to find the device in the fabric.

    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u32 PortID                       :   24 bit port ID to register protocol
        u32 Protocol                     :   32 bit bit map of protocols to register
        u32 ResponseBufferPA             :   PCI physical address of the buffer to put the response in
        u32 ResponseBufferLength         :   Size of response buffer in bytes.  Should be allocated to 16 bytes
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 on success, 1 otherwise
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:
    The data that is returned in the response buffer consists of a 16 byte header

    typedef struct                          define Fabric controller SNS map fabric response
    {
        u8 Header[16];                      16 bit word at offset 8 is completion status, 8002h = success, 8001h = fail (Big Endian format)
    }SNSResponseHeaderType;


******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Fabric_Register_Protocol(u8 HostID, u32 PortID, u32 Protocol, u32 ResponseBufferPA, u32 ResponseBufferLength,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        SNS_RFT_ID_Type *SNSCommand;

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 
            u32 CommandBufferQOffset = HostAdapter->MBCQueueHead*MAILBOX_COMMAND_BUFFER_QUEUE_SIZE;
            
            if (HostAdapter->MBCQueueNumEntries == MailboxCommandQueueSize) /* then queue full so quit here */
            {
                Fibre_Release(HostID);
                return FT_OUTSTANDING_REQUEST;
            }

            SNSCommand = (SNS_RFT_ID_Type *) HostAdapter->MBCommandBufferQ+CommandBufferQOffset;
                         /* need to allocate a different Command buffer for each command that uses buffer as input to command */

            SNSCommand->ResponseLength16    = ResponseBufferLength/2;
            SNSCommand->Reserved1           = 0;
            SNSCommand->ResponseBufPALow    = ResponseBufferPA;
            SNSCommand->ResponseBufPAHigh   = 0;
            SNSCommand->SubCommandLength    = SNS_RFT_ID_SUBCMD_LENGTH16;
            SNSCommand->Reserved2           = 0;
            SNSCommand->SubCommand          = SNS_RFT_ID_CMD;
            SNSCommand->Reserved3           = 0;
            SNSCommand->Reserved4           = 0;
            SNSCommand->PortID              = PortID;
            SNSCommand->FC4_Type[0]         = Protocol;
            SNSCommand->FC4_Type[1]         = 0;
            SNSCommand->FC4_Type[2]         = 0;
            SNSCommand->FC4_Type[3]         = 0;
            SNSCommand->FC4_Type[4]         = 0;
            SNSCommand->FC4_Type[5]         = 0;
            SNSCommand->FC4_Type[6]         = 0;
            SNSCommand->FC4_Type[7]         = 0;


#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_RFT_ID_Type)/4, 0, 0, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_RFT_ID_Type));
#endif

            MBCQueueItem.MBCState              = SNSSent;				
            MBCQueueItem.Callback              = CallBackFunction;	
            MBCQueueItem.ClientID              = CommandID;

            MBCQueueItem.Mailbox[0]            = MBC_SEND_SNS;	
            MBCQueueItem.Mailbox[1]            = SNS_RFT_ID_CMD_LENGTH16;			
            MBCQueueItem.Mailbox[2]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset) >> 16;			
            MBCQueueItem.Mailbox[3]            = (HostAdapter->MBCmdBufferQPA+CommandBufferQOffset);			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;       
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = SNSSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;

        SNSCommand = (SNS_RFT_ID_Type *) HostAdapter->MBCommandBuffer;

        SNSCommand->ResponseLength16    = ResponseBufferLength/2;
        SNSCommand->Reserved1           = 0;
        SNSCommand->ResponseBufPALow    = ResponseBufferPA;
        SNSCommand->ResponseBufPAHigh   = 0;
        SNSCommand->SubCommandLength    = SNS_RFT_ID_SUBCMD_LENGTH16;
        SNSCommand->Reserved2           = 0;
        SNSCommand->SubCommand          = SNS_RFT_ID_CMD;
        SNSCommand->Reserved3           = 0;
        SNSCommand->Reserved4           = 0;
        SNSCommand->PortID              = PortID;
        SNSCommand->FC4_Type[0]         = Protocol;
        SNSCommand->FC4_Type[1]         = 0;
        SNSCommand->FC4_Type[2]         = 0;
        SNSCommand->FC4_Type[3]         = 0;
        SNSCommand->FC4_Type[4]         = 0;
        SNSCommand->FC4_Type[5]         = 0;
        SNSCommand->FC4_Type[6]         = 0;
        SNSCommand->FC4_Type[7]         = 0;

#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)SNSCommand, sizeof(SNS_RFT_ID_Type)/4, 0, 0, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) SNSCommand, sizeof(SNS_RFT_ID_Type));
#endif

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SEND_SNS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, SNS_RFT_ID_CMD_LENGTH16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

/* *******************************************************************************************
    Fibre_Register_RDMA

    Description:
        Sets the base address of the Remote DMA buffer
        Returns FIBRE_TRANSFER_ERR type

    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 BufferBasePA    : The physical (PCI) address of the RDMA buffer
        u32 ID              : User specified ID passed to callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Register_RDMA(u8 HostID, u32 BufferBasePA, u32 ID,
                                      void (*NotificationHandler)(TransferInfoType *),
                                      void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;


    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = SetRDMABaseSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_SET_RDMA_BASE;			
            MBCQueueItem.Mailbox[1]            = BufferBasePA & 0xFFFF;			
            MBCQueueItem.Mailbox[2]            = BufferBasePA >> 16;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            
            HostAdapter->RDMANotification      = NotificationHandler;   /* can only be one so no need to queue */
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->RDMANotification           = NotificationHandler;
        HostAdapter->MBCState                   = SetRDMABaseSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SET_RDMA_BASE);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, BufferBasePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, BufferBasePA >> 16);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}


/* Added for enhanced RDMA */

/* *******************************************************************************************
    Fibre_Register_RDMA_SubAddr

    Description:
        Sets the base address of the Remote DMA buffer, or subaddress 
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 BufferBasePA    : The physical (PCI) address of the RDMA buffer
        u32 length			: The length of the buffer (only 24 bit arguments allowed!)
		u8 flags			: enumerated type defining the buffers read/write capabilities
		u8 subaddress		: the reference number for the subaddress (0-255)
        u32 ID              : User specified ID passed to callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Register_RDMA_SubAddr(u8 HostID, u32 BufferBasePA, u32 length,
									  u8 flags, u8 subaddress, u32 ID,
                                      void (*NotificationHandler)(TransferInfoType *),
                                      void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;


    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;

    if (length & 0xf0000000) /* length > 64MB-1 */
        Status = FT_INVALID_BUFFER_SIZE;

    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = SetRDMASubAddrSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_SET_RDMA_SUBADDR;			
            MBCQueueItem.Mailbox[1]            = BufferBasePA & 0xFFFF;			
            MBCQueueItem.Mailbox[2]            = BufferBasePA >> 16;
            MBCQueueItem.Mailbox[3]            = length & 0xFFFF;            
            MBCQueueItem.Mailbox[6]            = (flags<<8)|(length >> 16);
            MBCQueueItem.Mailbox[7]            = subaddress;
            			
            MBCQueueItem.NumMailboxRegs        = 6;       
            
            HostAdapter->RDMANotification      = NotificationHandler;   /* can only be one so no need to queue */
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }   

        HostAdapter->RDMANotification           = NotificationHandler;    
        HostAdapter->MBCState                   = SetRDMASubAddrSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SET_RDMA_SUBADDR);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, BufferBasePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, BufferBasePA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, length & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, ((flags<<8)|(length >> 16)));
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, subaddress);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */
    
    return Status;
}


/* *******************************************************************************************
    Fibre_Register_RDMA_Multiple

    Description:
        Sets the base address of the Remote DMA buffer 
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 BufferBasePA    : The physical (PCI) address of the RDMA buffer
        u32 ID              : User specified ID passed to callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Register_RDMA_Multiple(u8 HostID, u8 *TablePtr, u32 TablePA, u32 ID,
                                      void (*NotificationHandler)(TransferInfoType *),
                                      void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;


    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            /* don't want to big e convert if can't be used so check to see if queue full first */
            if (HostAdapter->MBCQueueNumEntries == MailboxCommandQueueSize) /* then queue full so quit here */
            {
                Fibre_Release(HostID);
                return FT_OUTSTANDING_REQUEST;
            }

            MBCQueueItem.MBCState              = SetRDMAMultipleSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_SET_RDMA_BASE;			
            MBCQueueItem.Mailbox[1]            = TablePA & 0xFFFF;			
            MBCQueueItem.Mailbox[2]            = TablePA >> 16;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            
            HostAdapter->RDMANotification      = NotificationHandler;   /* can only be one so no need to queue */

#ifdef BIG_ENDIAN
		    {
	            int i=0;
	            u32 *Table = (u32 *)TablePtr; 
	            BigEndConvert(Table, MaxSubaddresses*2, FC_FALSE, FC_FALSE, FC_FALSE);
		        for	(i=0;i<MaxSubaddresses*2;i++)
		        {
			        if (!(i%2))
				        *((Table)+i) = word_swap32(*((Table)+i));
		        }
		    }
#endif		
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }   

#ifdef BIG_ENDIAN
		{
	        int i=0;
	        u32 *Table = (u32 *)TablePtr; 
	        BigEndConvert(Table, MaxSubaddresses*2, FC_FALSE, FC_FALSE, FC_FALSE);
		    for	(i=0;i<MaxSubaddresses*2;i++)
		    {
			    if (!(i%2))
				    *((Table)+i) = word_swap32(*((Table)+i));
		    }
		}
#endif		
        HostAdapter->RDMANotification           = NotificationHandler;    
        HostAdapter->MBCState                   = SetRDMAMultipleSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SET_RDMA_MULTIPLE);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, TablePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, TablePA >> 16);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Get_SubAddr_List

    Description:
        Gets the list of the Remote DMA buffer subaddresses and their properties 
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 LsitAddress     : The  address the RDMA buffer is to be written to
        u32 ID              : User specified ID passed to callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Get_SubAddr_List(u8 HOST_ID, u32 *ListPtr, u32 ListAddress, u32 CommandID, 
										void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    u16 AddrLSW = (ListAddress & 0xFFFF);


    if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetSubAddrListSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_SUBADDR_LIST;			
            MBCQueueItem.Mailbox[1]            = AddrLSW;			
            MBCQueueItem.Mailbox[2]            = ListAddress >> 16;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 5;       
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }   

        HostAdapter->MBCState                   = GetSubAddrListSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_SUBADDR_LIST);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, AddrLSW);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, ListAddress >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Get_SubAddr_Single

    Description:
        Gets the list of the Remote DMA buffer subaddresses and their properties 
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 LsitAddress     : The  address the RDMA buffer is to be written to
        u32 ID              : User specified ID passed to callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Get_SubAddr_Single(u8 HOST_ID, u8 SubAddress, u32 CommandID, 
										void (*CallBackFunction)(u16 Status, u32 Id, u16 Flgs_LenMSB, u16 LenLSW, u16 AddrLSW, u16 AddrMSW))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;


    if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetSubAddrSingleSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_SUBADDR_SINGLE;			
            MBCQueueItem.Mailbox[1]            = SubAddress;			
            MBCQueueItem.NumMailboxRegs        = 2;       
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }   

        HostAdapter->MBCState                   = GetSubAddrSingleSent;
        HostAdapter->MBCCallBackFunction        = (void(*))CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_SUBADDR_SINGLE);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, SubAddress);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* end addition for enhanced RDMA */

/* *******************************************************************************************
    Fibre_LoopBack_Test

    Description:
        Performs a 2K byte loopback test - Checks for low level CRC, disparity, frame length error
        User is responisble for data test.
        Returns FIBRE_TRANSFER_ERR type

    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 RXBufferBasePA  : The physical (PCI) address of the receive data buffer (LOOPBACK_TEST_LENGTH bytes)
        u32 TXBufferBasePA  : The physical (PCI) address of the transmit data buffer LOOPBACK_TEST_LENGTH bytes)
        u32 IterationCount  : The number of times to run the test.
        Boolean External    : If true then perform external loopback test, else perform internal test
        u32 ID              : User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Command Error
                    2 = LoopBack Error
                    3 = Loop down

                    2-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_LoopBack_Test(u8 HostID, u32 RXBufferBasePA, u32 TXBufferBasePA,
                                       u32 IterationCount, Boolean External, u32 Size, u32 ID,
                                       void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = LoopBackTestSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_LOOPBACK_TEST;			
            MBCQueueItem.Mailbox[1]            = External ? 0x32 : 0x31;			
            MBCQueueItem.Mailbox[10]           = Size & 0xFFFF;			
            MBCQueueItem.Mailbox[11]           = Size >> 16;			
            MBCQueueItem.Mailbox[12]           = 0;			
            MBCQueueItem.Mailbox[13]           = 0;			
            MBCQueueItem.Mailbox[14]           = TXBufferBasePA & 0xFFFF;			
            MBCQueueItem.Mailbox[15]           = TXBufferBasePA >> 16;			
            MBCQueueItem.Mailbox[16]           = RXBufferBasePA & 0xFFFF;			
            MBCQueueItem.Mailbox[17]           = RXBufferBasePA >> 16;			
            MBCQueueItem.Mailbox[18]           = IterationCount & 0xFFFF;			
            MBCQueueItem.Mailbox[19]           = IterationCount >> 16;			
            MBCQueueItem.NumMailboxRegs        = 2;  /* count of mailboxes 7 and lower */     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = LoopBackTestSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_LOOPBACK_TEST);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, External ? 0x32 : 0x31);
        FCWrite16(HostAdapter->BaseAddress + Mailbox10, Size & 0xFFFF); /* transfer count fixed at 2K per Qlogic */
        FCWrite16(HostAdapter->BaseAddress + Mailbox11, Size >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox12, 0);                 /* no RX segments since only 2K          */
        FCWrite16(HostAdapter->BaseAddress + Mailbox13, 0);                 /* no TX segments since only 2K          */
        FCWrite16(HostAdapter->BaseAddress + Mailbox14, TXBufferBasePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox15, TXBufferBasePA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox16, RXBufferBasePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox17, RXBufferBasePA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox18, IterationCount & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox19, IterationCount >> 16);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

/* *******************************************************************************************
    Fibre_Diagnostic_Echo_Test

    Description:
        Performs a 2K byte loopback test - Checks for low level CRC, disparity, frame length error
        User is responisble for data test.
        Returns FIBRE_TRANSFER_ERR type

    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u16 DataLength		: The amount of data to be sent and echoed (0-220 bytes)
        u32 RXBufferBasePA  : The physical (PCI) address of the receive data buffer (LOOPBACK_TEST_LENGTH bytes)
        u32 TXBufferBasePA  : The physical (PCI) address of the transmit data buffer LOOPBACK_TEST_LENGTH bytes)
        u32 IterationCount  : The number of times to run the test.
        u32 ID              : User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Command Error
                    2 = LoopBack Error
                    3 = Loop down

                    2-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Diagnostic_Echo_Test(u8 HostID, u16 DataLength, u32 RXBufferBasePA, 
                                       u32 TXBufferBasePA ,u32 ID,
                                       void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = DiagnosticEchoSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = ID;             
            MBCQueueItem.Mailbox[0]            = MBC_DIAGNOSTIC_ECHO;			
            MBCQueueItem.Mailbox[1]            = 0x8000;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.Mailbox[10]           = DataLength;			
            MBCQueueItem.Mailbox[11]           = 0;			
            MBCQueueItem.Mailbox[12]           = 0;			
            MBCQueueItem.Mailbox[13]           = 0;			
            MBCQueueItem.Mailbox[14]           = TXBufferBasePA & 0xFFFF;			
            MBCQueueItem.Mailbox[15]           = TXBufferBasePA >> 16;			
            MBCQueueItem.Mailbox[16]           = RXBufferBasePA & 0xFFFF;			
            MBCQueueItem.Mailbox[17]           = RXBufferBasePA >> 16;			
            MBCQueueItem.Mailbox[18]           = 0;			
            MBCQueueItem.Mailbox[19]           = 0;			
            MBCQueueItem.Mailbox[20]           = 0;			
            MBCQueueItem.Mailbox[21]           = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;  /* count of mailboxes 7 and lower must account for 2 and 3 also*/     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = DiagnosticEchoSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_DIAGNOSTIC_ECHO);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, 0x8000);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox10, DataLength); 
        FCWrite16(HostAdapter->BaseAddress + Mailbox14, TXBufferBasePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox15, TXBufferBasePA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox16, RXBufferBasePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox17, RXBufferBasePA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox20, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox21, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

#ifdef IP_SUPPORT
/* *******************************************************************************************
    Fibre_Enable_IP_Protocol

    Description:
        Enables and configures the IP protocol processing within the NIC
        Returns FIBRE_TRANSFER_ERR type

    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u16 MTU             : Maximum Transfer Unit Size in bytes - MAX 65280, min 92
        u16 IPHeaderSize    : Size of IP header to isolate in separate receive buffer
        u16 RxBufferSize    : Size of Data buffers used to receive data in
                              Typically <= MTU, must be page size in paged memory systems
                              Tradeoff of wasted memory for small transfers vs number of buffers needed
                              if MTU = 32K and Buffer size = 4K then one 32K rx will be placed into 8
                              rx buffers. There is no guarantee that these buffers will be contiguous
        u32 ID              : User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the completion Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Enable_IP_Protocol(u8 HostID, u16 MTU, u16 IPHeaderSize,
                                            u16 RxBufferSize, u32 ID,
                                            void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        /* don't want to process if can't be queued so check to see if queue full first */
        if (HostAdapter->MBCQueueNumEntries == MailboxCommandQueueSize) /* then queue full so quit here */
        {
            Fibre_Release(HostID);
            return FT_OUTSTANDING_REQUEST;
        }

        InitializeIP(HostAdapter, MTU, IPHeaderSize, RxBufferSize, ID, CallBackFunction);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}
#endif

/* *******************************************************************************************
    Fibre_Enable_Watchdog  (2200 special builds only )
    Description:
        Enables and sets the watchdog timer in the tirmware.  Each timer tick is 116 usec.
        Use WATCHDOG_USEC_PER_TICK definition.
        Returns FIBRE_TRANSFER_ERR type

    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u16 Timeout         : The number of 116 usec ticks until a timeout - 7.6 seconds max
        u32 ID              : User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Enable_Watchdog(u8 HostID, u16 Timeout, u32 ID, void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            Fibre_Release(HostID);
            return FT_OUTSTANDING_REQUEST;
        }
        HostAdapter->MBCState                   = EnableWDCSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_ENABLE_WATCHDOG);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, Timeout);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Reset_Error_Statistics(u8 HostID, u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Request the Loop Error Status Block from the specified Loop ID.  Loop ID may be the ID of the local node.
        The LESB structure is placed in the LESBBuffer

    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 on success, 1 otherwise
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Reset_Error_Statistics(u8 HostID, u32 CommandID, 
												void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = ResetLESBSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_RESET_LINK_STATUS;			
            MBCQueueItem.NumMailboxRegs        = 1;  /* count of mailboxes 7 and lower */     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = ResetLESBSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;


        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_RESET_LINK_STATUS);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Reset_Error_Statistics(u8 HostID, u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Request the Loop Error Status Block from the specified Loop ID.  Loop ID may be the ID of the local node.
        The LESB structure is placed in the LESBBuffer

    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 = 1 Gigabit connection, 1 = 2 gigabit connection
                                             any other value is command error
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Get_Link_Data_Rate(u8 HostID, u32 CommandID, 
												void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetDataRateSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_DATA_RATE;	
            MBCQueueItem.Mailbox[1]            = 0;     /* get current data rate */		
            MBCQueueItem.NumMailboxRegs        = 2;     /* count of mailboxes 7 and lower */     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = GetDataRateSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;


        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_DATA_RATE);  
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, 0);   /* get current data rate */	
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}


/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Driver_Heartbeat(u8 HostID, u8 LoopID, LESBType *LESB,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Request the Loop Error Status Block from the specified Loop ID.  Loop ID may be the ID of the local node.
        The LESB structure is placed in the LESBBuffer

    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u8  HBInterval                   :   The interval (0-4095 seconds) between driver heartbeats sent to fw
                                             (i.e. successive calls to this function)  setting the interval to 0 will cancel the HB  
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 on success, 1 otherwise
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Driver_Heartbeat(u8 HostID, u16 HBInterval, u32 CommandID, 
											void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = DriverHBSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_DRIVER_HB;			
            MBCQueueItem.Mailbox[1]            = HBInterval;			
            MBCQueueItem.NumMailboxRegs        = 2;  /* count of mailboxes 7 and lower */     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = DriverHBSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_DRIVER_HB);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, HBInterval);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}


/* *******************************************************************************************
FIBRE_TRANSFER_ERR Fibre_Firmware_Heartbeat(u8 HostID, u8 LoopID, LESBType *LESB,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
    Description:

        Request the Loop Error Status Block from the specified Loop ID.  Loop ID may be the ID of the local node.
        The LESB structure is placed in the LESBBuffer

    Parameters:
        u8  HostID                       :   The ID assigned to the HostAdapter
        u8  HBInterval                   :   The interval (in ms) between firmware heartbeats
        LESB HBTargetPA                  :   Physical Address of the coounter (16-bit) sent at each heartbeat from firmware
                                             setting this to 0 will cancel the heartbeat
        u32 CommandID                    :   User specified command ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 CommandID) :   Pointer to the Callback Function
                Return Value:
                        u32  Status      :   0 on success, 1 otherwise
                        u32 CommandID    :   User specified command ID passed to callback function
    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The callback function will not be
                                                   called be if error status is returned here.

    Notes:

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Firmware_Heartbeat(u8 HostID, u16 HBInterval, u32 HBTargetPA,
                                    u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = FirmwareHBSent;				
            MBCQueueItem.Callback              = CallBackFunction;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_FIRMWARE_HB;			
            MBCQueueItem.Mailbox[1]            = HBInterval;			
            MBCQueueItem.Mailbox[2]            = HBTargetPA >> 16;			
            MBCQueueItem.Mailbox[3]            = HBTargetPA;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;  /* count of mailboxes 7 and lower */     
             
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = FirmwareHBSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_FIRMWARE_HB);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, HBInterval);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HBTargetPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HBTargetPA);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}




/* *******************************************************************************************
    Fibre_Send_Mailbox_Command

    Description:
        Generic Mailbox command function. 
        Returns FIBRE_TRANSFER_ERR type

    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u16 MB0, MB1, MB2, MB3, MB6, MB7   : Values to be placed in associated Mailbox registers
        u32 ID              : User specified ID passed to callback function
        void (*CallBackFunction)(u16 MB0, u16 MB1, u16 MB2): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status.

    Notes:
        Callback parameters:


******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Send_Mailbox_Command(u8 HostID, u16 MB0, u16 MB1, u16 MB2, u16 MB3, u16 MB6, u16 MB7,
                                              void (*CallBackFunction)(u16 MB0, u16 MB1, u16 MB2, u16 MB3, u16 MB6, u16 MB7))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GenericMBCSent;				
            MBCQueueItem.Callback              = (void(*))CallBackFunction;				
            MBCQueueItem.ClientID              = 0;             
            MBCQueueItem.Mailbox[0]            = MB0;			
            MBCQueueItem.Mailbox[1]            = MB1;			
            MBCQueueItem.Mailbox[2]            = MB2;			
            MBCQueueItem.Mailbox[3]            = MB3;			
            MBCQueueItem.Mailbox[6]            = MB6;			
            MBCQueueItem.Mailbox[7]            = MB7;			
            MBCQueueItem.NumMailboxRegs        = 6;  /* count of mailboxes 7 and lower */     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = GenericMBCSent;
        HostAdapter->MBCCallBackFunction        = (void(*))CallBackFunction;
        HostAdapter->MBCClientID                = 0;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MB0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, MB1);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, MB2);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, MB3);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, MB6);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, MB7);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

#ifdef IP_SUPPORT

/* *******************************************************************************************
    Fibre_Register_Fast_IP_Buffer

    Description:
        Sets the base address of the fast IP buffer
        Returns FIBRE_TRANSFER_ERR type

    Parameters:
        u8 HostID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 BufferBasePA    : The physical (PCI) address of the IP buffer
        u16 NumberOfBlocks  : The number of blocks in the IP buffer
        u32 ID              : User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.

            ID - the 32 bit ID assigned to this command

        The IP interface must have been previously initialized. Performed in Fibre_Intialization
        The size of the blocks is defined by the MTU size set in IP intialization

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Register_Fast_IP_Buffer(u8 HostID, u32 BufferBasePA, u16 NumberOfBlocks, u32 ID,
                                       void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;
    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else if (!HostAdapters[HostAdapterIndex].IPInitialized)
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow command if outstanding Mailbox command */
        {
            Fibre_Release(HostID);
            return FT_OUTSTANDING_REQUEST;
        }
        HostAdapter->MBCState                   = SetIPFastBufSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_REGISTER_FAST_IP_BUFFER);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, BufferBasePA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, BufferBasePA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, NumberOfBlocks);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

#endif


/* *******************************************************************************************
    Fibre_Define_Implicit_Port

    Description:
        Implicitly forces a port database entry for the specified port

  **** only available on 2200 *****

    Parameters:
        u8  HostID              :            The host adapter ID
        u8  LoopID              :            Loop ID to be assigned to this remote port
        u32 RemotePortID        :            The 24 bit port ID of the remote port
        u16 RemoteFrameSize     :            The max frame size that the remote port can receive
        u16 RemoteControlOptions:            The Options for the remote port
                7    (Reserved)
                6    POC_ACK0 (Class 2)
                     When set to a one, the recipient is capable of supporting ACK_0.
                5    (Reserved)
                4    POC_C2_SUP
                     When set to a one, the recipient supports class 2 services.
                3    POC_ACK0_I (Class 2)
                     When set to a one, the initiator is capable of supporting ACK_0.
                2    POC_ACK_GEN_A (Class 2)
                     When set to a one, the initiator is capable of generating the
                     ACK_Form bits of the  F_CTL field.
                1-0 (Reserved)
        u32 ID                  :            User specified ID passed to callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status.

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.

            ID - the 32 bit ID assigned to this command

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Define_Implicit_Port(u8 HostID, u8 LoopID, u32 RemotePortID, u16 RemoteFrameSize,
                                              u16 RemoteControlOptions, u32 ID,
                                              void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;                                        /* location of Adapter assigned to HOST_ID  */
    HostAdapterType *HostAdapter;
    ImplicitPortDefType PortDef;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                         */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                               /* adapter not in list                  */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];                  /* setup access variables               */
        if (HostAdapter->MBCState != NoMBCSent)               /* don't allow command if outstanding Mailbox command */
        {
            Fibre_Release(HostID);
            return FT_OUTSTANDING_REQUEST;
        }
        PortDef.PortID      = RemotePortID;
        PortDef.Control     = RemoteControlOptions;
        PortDef.RxDataSize  = RemoteFrameSize;
        *((ImplicitPortDefType *) HostAdapter->MBCommandBuffer) = PortDef; /* copy values to working buffer for DMA    */

#ifdef BIG_ENDIAN
        BigEndConvert((u32 *)HostAdapter->MBCommandBuffer, IMPLICITDEFWORDS, IMPLICITDEFSENSE, IMPLICITDEFCDB, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) HostAdapter->MBCommandBuffer, sizeof(PortDef));
#endif

        HostAdapter->MBCState                   = ImplicitPortSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = ID;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_IMPLICIT_PORT_DEF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, LoopID);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, 0);              /* 32 bit addressing for now                */
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, HostAdapter->MBCmdBufferPA & 0xFFFF);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;

} /* end Fibre_Define_Implicit_Port */


/* *******************************************************************************************
    Fibre_IntDisable(BaseAddressType BaseAddress);


    Description:
        Disables the PCI interrupt from the NIC at the specified base address.  This is provided
        for drivers in systems that can't disable the interrupt at a higher level.  PCI bus accesses
        costly in heavily loaded systems.  Every effort should be made to avoid having to disable
        interrupts at the card (across the PCI bus)

     Parameters:
        BaseAddressType BaseAddress :   The base address of the NIC

    Return Value:
        None
******************************************************************************************* */

void Fibre_IntDisable(BaseAddressType BaseAddress)
{
    FCWrite16(BaseAddress + Isp2PciIntCtl,  0); /* disable RISC Interrupt to PCI */
}

/* *******************************************************************************************
    Fibre_IntEnable(BaseAddressType BaseAddress);


    Description:
        re-enables the PCI interrupt from the NIC at the specified base address.  This is provided
        for drivers in systems that can't disable the interrupt at a higher level.  PCI bus accesses
        are costly in heavily loaded systems.  Every effort should be made to avoid having to re-enable
        interrupts at the card (across the PCI bus)

     Parameters:
        BaseAddressType BaseAddress :   The base address of the NIC

    Return Value:
        None
******************************************************************************************* */

void Fibre_IntEnable(BaseAddressType BaseAddress)
{
    FCWrite16(BaseAddress + Isp2PciIntCtl, ENABLE_ALL_INTS | ENABLE_RISC_INT); /* Enable RISC Interrupt to PCI */
}


#ifdef IP_SUPPORT
/* not required when using fast IP buffering which is only available for 2200. so 23xx should use this */

/* *******************************************************************************************
    Fibre_Add_IP_Buffers

    Description:
        Adds more IP receive buffer containers to the queue

    Parameters:
        u8  HostID:                             The host adapter ID
        UserIPBufContainerType *ContainerPtr:   Pointer to the start of the container list
        u8 NumBuffers:                          The number of containers in the list
    Return Value:
        FIBRE_TRANSFER_ERR:                     Status enumeration

    Notes:
        The user is responsible for not overflowing the Queue.  They should fill initially and then
        only replace what was returned.

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Add_IP_Buffers(u8 HostID, UserIPBufContainerType *ContainerPtr, u8 NumBuffers)
{
    u8 HostAdapterIndex;                                        /* location of Adapter assigned to HOST_ID  */
    HostAdapterType *HostAdapter;
    IPBufContainerType *Container;
    u8 i;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                         */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                               /* adapter not in list                  */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];                  /* setup access variables               */

        for (i = 0; i < NumBuffers; i++, ContainerPtr++)
        {
            Container = (IPBufContainerType *)NextIPRcvBufEntryAddress(HostAdapter);
            Container->ReceivePtrPALo = ContainerPtr->ReceivePtrPALo;
            Container->ReceivePtrPAHi = ContainerPtr->ReceivePtrPAHi;
            Container->BufferHandle   = ContainerPtr->BufferHandle;
 /* Relying on user to not overflow the queue */
#ifdef BIG_ENDIAN
    BigEndConvert((u32 *)Container, (sizeof(IPBufContainerType))/4, FC_FALSE, FC_FALSE, FC_FALSE);
#endif
            HostAdapter->IPRcvQHeadIndex++;
            HostAdapter->IPRcvQHeadIndex %= NumIPRcvBufQEntries;
        }

        /* update Q in pointer */
        FCWrite16(HostAdapter->BaseAddress + Mailbox8, HostAdapter->IPRcvQHeadIndex);
    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;

} /* Fibre_Add_IP_Buffers */

/* *******************************************************************************************
    Fibre_Register_FARP_Handler

    Description:
        Registers a handler for target commands that have no buffers registers with the FCRM.

    Parameters:
        u8 HOST_ID:                     Adapter ID
        void (*TARGET_HANDLER)(NotificationType *)): Callback function to handle unregistered Target commands

    Return Value:
        FIBRE_TRANSFER_ERR:             Error enumeration

    Notes:

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Register_FARP_Handler(u8 HostID, void (*FARPHandler)(FARPInfoType *))
{
    u8 HostAdapterIndex;
    FIBRE_TRANSFER_ERR TransferErr = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                                     */
        return FT_INVALID_HOST_ID;

    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list  */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                       /* adapter not in list                                  */
        TransferErr = FT_DEVICE_NOT_INITIALIZED;
    else
        HostAdapters[HostAdapterIndex].FARPHandler = FARPHandler;   /* assign target handler                        */

    Fibre_Release(HostID);

    return TransferErr;
}

/* *******************************************************************************************
    Fibre_Send_FARP_Protocol

    Description:
        Registers a handler for target commands that have no buffers registers with the FCRM.

    Parameters:
        u8 HOST_ID:                     Adapter ID
        void (*TARGET_HANDLER)(NotificationType *)): Callback function to handle unregistered Target commands

    Return Value:
        FIBRE_TRANSFER_ERR:             Error enumeration

    Notes:

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Send_FARP_Protocol(FARPInfoType *FARPInfo)
{
    u8 HostAdapterIndex;
	u8 HostID = FARPInfo->HostID;
    FIBRE_TRANSFER_ERR TransferErr = FT_SUCCESS;
    HostAdapterType *HostAdapter;

    if (HostID > PORT_ID_MAX)                                   /* validate host id                                     */
        return FT_INVALID_HOST_ID;

    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list  */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                       /* adapter not in list                                  */
        TransferErr = FT_DEVICE_NOT_INITIALIZED;
	else
	{
        HostAdapter = &HostAdapters[HostAdapterIndex];              /* setup access variables               */
        SendFARPPacket(FARPInfo, HostAdapter);
    }
    Fibre_Release(HostID);

    return TransferErr;
}

#endif /* ifdef IP_SUPPORT */

FIBRE_TRANSFER_ERR Fibre_I2C_A_D_Read(u8 HostID, A2DChannelType Channel, u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    u8  PhysicalChannel[8] = { 0, 4, 1, 5, 2, 6, 3, 7};
    u8 A2DCommand = 0x8C | (PhysicalChannel[Channel] << 4);
    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetSensorA_D_Sent;             
            MBCQueueItem.Callback              = CallBackFunction;              
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_SENSOR_A_D;            
            MBCQueueItem.Mailbox[1]            = A2DCommand;            
            MBCQueueItem.NumMailboxRegs        = 1;  /* count of mailboxes 7 and lower */     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = GetSensorA_D_Sent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;


        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_SENSOR_A_D);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, A2DCommand);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

FIBRE_TRANSFER_ERR Fibre_I2C_TempRead(u8 HostID, u32 CommandID, void (*CallBackFunction)( u32 Status, u32 CommandID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HostID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {

        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)           /* don't allow close if outstanding Mailbox command */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetSensorTempSent;             
            MBCQueueItem.Callback              = CallBackFunction;              
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_SENSOR_TEMP;           
            MBCQueueItem.NumMailboxRegs        = 1;  /* count of mailboxes 7 and lower */     
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
        HostAdapter->MBCState                   = GetSensorTempSent;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
        HostAdapter->MBCClientID                = CommandID;


        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_SENSOR_TEMP);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);

    } /* end else host adapter initialized */

    Fibre_Release(HostID);

    /* Mutual exclusion ends here   */

    return Status;
}

#ifdef ISP24XX
/* ISP24XX specific selftest */
/* *******************************************************************************************
    FibreISP24XXSelfTest

    Description:
        Performs a invasive selftest of the FC-24xx host adapter

    Parameters:
        BaseAddressType BASE_ADDRESS:   Address of the Host adapter
        u16 DeviceID:                   Device ID from PCI configuration space 0x2100, 0x2200, 0x2300, 0x2312
        Boolean SkipRAMTest:            If true, RAM test will be skipped - Faster execution
        u8 *RAMTestBuffer:              A 64k BYTE buffer used for ram testing.  Only required for 2300s and up
        u32 RAMTestBufferPA:            The PCI address of the start of the ram test buffer
        u32 RAMSize:                    Number of BYTES of RAM on the card to test
        u32 *FirstFailAddress:          A pointer to a u32.  This will contain the address of the first ram failure if one occurs,
                                        otherwise it will be set to 0
                                        If SkipRAMTest is True or RAMSize is 0, RAMTestBuffer, RAMTestBufferPA and FirstFailAddress
                                        are not used and only 128K words are tested on a 2300

    Return Value:
        FIBRE_TEST_ERR:                 Status bit map

    Notes:
        Application is not allowed to call Fibre_Initialization for the same adapter until self test completes
******************************************************************************************* */

FIBRE_TEST_ERR FibreISP24XXSelfTest(BaseAddressType BaseAddress, u16 DeviceID, Boolean SkipRAMTest,
                                    u8 *RAMTestBuffer, u32 RAMTestBufferPA, u32 RAMSize, u32* FirstFailAddress)
{
    u16 Value, ReadValue;                                       /* Ram Test data                            */
    u16 Mailbox;
    u32 i;
    u16 RamLength, RamAddress;
    u16 Checksum;
    volatile u32 Counter;
    u16 FPMHWRev;
    HostAdapterType *HostAdapter;

    BaseAddressType HostAdapterAddress = BaseAddress;
    u8 HostAdapterIndex;



FCDEBUGPRINT(("Self test \n"));
    RAMSize = RAMSize/2;   /* convert to 16 bit words */

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++) /* Make sure not already initialized */
        if (HostAdapters[HostAdapterIndex].Initialized == FC_TRUE && HostAdapters[HostAdapterIndex].BaseAddress == BaseAddress)
            return FB_DEVICE_NOT_CLOSED;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++) /* find a slot to put it */
        if (HostAdapters[HostAdapterIndex].Initialized == FC_FALSE)
           break;

    if (HostAdapterIndex >= MAX_ADAPTERS)        /* make sure we can add a new adapter   */
        return (FI_TOO_MANY_ADAPTERS);

    HostAdapter = &HostAdapters[HostAdapterIndex];
    HostAdapter->BaseAddress = BaseAddress;
    HostAdapter->StatusHandler = FC_NULL;


    {
        u32 Counter;
        u16 MB0, MB3;
        
        FCWrite32(BaseAddress + IspCtlSts, 0x10000);            /* Shutdown any DMA activity */
        Counter = 0;
        while ((FCRead32(BaseAddress + IspCtlSts) & 0x20000) && (Counter++ < 100000));
        if (Counter >= 100000)                                  /* Then DMA never stopped    */
            return (FI_DEVICE_NOT_FOUND);
        
        FCWrite16(BaseAddress + IspCtlSts, 0x10001);            /* hardware reset  and DMA shut down */
        FCDelay(FC2100ResetDelay);
        Counter = 0;
        while ((FCRead16(BaseAddress + IspCtlSts) & 0x01) && (Counter++ < 100000));
        if (Counter >= 100000)                                  /* Then never reset          */
            return (FI_DEVICE_NOT_FOUND);
    
        FCDelay(20*FC2100ResetDelay);
    
        /* wait for reset completion and then verify chip there */       
    
        MB0 = FCRead16(BaseAddress + Mailbox0);
        while ((FCRead16(BaseAddress + Mailbox0) != 0) && (Counter++ < 100000));
        if (Counter >= 100000)                                  /* Then never reset          */
            return (FI_DEVICE_NOT_FOUND);
        if (FCRead16(BaseAddress + Mailbox1) != 0x4953)  /* should be "IS" */
            return (FI_DEVICE_NOT_FOUND);
        if (FCRead16(BaseAddress + Mailbox2) != 0x5020)  /* should be "P " */
            return (FI_DEVICE_NOT_FOUND);
        MB3 = FCRead16(BaseAddress + Mailbox3);
        if (DeviceID == FC2422_DEVICE_ID)
        {
            if (MB3 != FC2422_DEVICE_ID) 
                return (FI_DEVICE_NOT_FOUND);
        }
        else
        {
            if (DeviceID == FC2432_DEVICE_ID)
            {
                if (MB3 != FC2432_DEVICE_ID) 
                    return (FI_DEVICE_NOT_FOUND);
            }
            else /* invalid device selected */
                return (FI_DEVICE_NOT_FOUND);
        }
    }        


//    RESET_RISCfn(HostAdapterAddress);     /* reset RISC                               */

//    FCDelay(FC2100ResetDelay);
//    RELEASE_RISCfn(HostAdapterAddress);       /* Allow boot firmware to run               */


    /* Mailbox Test */

    WaitForMboxReady(HostAdapterAddress);
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_MB_REG_TEST);  /* write patterns to inbound mailboxes      */
    FCWrite16(HostAdapterAddress + Mailbox1, 0x0000);
    FCWrite16(HostAdapterAddress + Mailbox2, 0x5555);
    FCWrite16(HostAdapterAddress + Mailbox3, 0xAAAA);
    FCWrite16(HostAdapterAddress + Mailbox4, 0xFFFF);
    FCWrite16(HostAdapterAddress + Mailbox5, 0xFA50);
    FCWrite16(HostAdapterAddress + Mailbox6, 0xA50F);
    FCWrite16(HostAdapterAddress + Mailbox7, 0xF05A);

    SET_HOST2RISC_INTR(HostAdapterAddress);  /* tell adapter to loop data to outbound mailboxes   */
    Mailbox = WaitForMboxCmdCmpltn(HostAdapter);
    if(Mailbox != MB_STATUS_GOOD)
        return (FB_MAILBOX_FAIL);
    else
    {
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox1)) != 0x0000)
             return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox2)) != 0x5555)
             return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox3)) != 0xAAAA)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox4)) != 0xFFFF)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox5)) != 0xFA50)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox6)) != 0xA50F)
            return (FB_MAILBOX_FAIL);
        if ((Mailbox =FCRead16(HostAdapterAddress + Mailbox7)) != 0xF05A)
            return (FB_MAILBOX_FAIL);
    }
#if 0 /* ***********************************************************************************************************************/
    if (!SkipRAMTest)
    {
        if (DeviceID < FC2300_DEVICE_ID || RAMSize == 0) /* if ramsize = 0 then only do basic RAM test on 2300 */
        {
            if (FPMHWRev <= 2) /* old 2100 */
            {
                /* RISC RAM Test */

                Value = 0;                                          /* test 0 fill pattern      */

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)
                    RISC_Access(i, RISC_WRITE, &Value, HostAdapter);

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)
                {
                    RISC_Access(i, RISC_READ, &Value, HostAdapter);
                    if (Value != 0)
                        return (FB_RAM_FAIL);
                }

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)    /* test counting pattern    */
                {
                    Value = i;
                    RISC_Access(i, RISC_WRITE, &Value, HostAdapter);
                }

                for (i = RISC_RAM_START; i < RISC_RAM_SIZE; i++)
                {
                    RISC_Access(i, RISC_READ, &Value, HostAdapter);
                    if (Value != i)
                        return (FB_RAM_FAIL);
                }
            }
            else /* 2100A or 2200 */
            {
                /* Speed improved RISC RAM Test -- takes 10's of msec compared to seconds       */
                /* we use the Boot loader's Init ram and checksum routines to speed up the test */


                Value      = 0x5555;                                                  /* test 5555 fill pattern                      */
                RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                RamAddress = RISC_RAM_START;

                while (RamLength > INIT_RAM_LENGTH)                                  /* can only set INIT_RAM_LENGTH words at a time*/
                {
                    InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                    RamAddress += INIT_RAM_LENGTH;
                    RamLength  -= INIT_RAM_LENGTH;
                }
                InitRiscRam(RamAddress, RamLength, Value, HostAdapter);  /* fake out checksum routine to think real code     */
                Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;          /* is present                                       */
                Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                Checksum = 0 - Checksum;
                Value = RISC_RAM_SIZE-RISC_RAM_START;
                RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);

                if (!VerifyRiscFwChecksum(HostAdapter))
                   return (FB_RAM_FAIL);

                Value      = 0xAAAA;                                                  /* test AAAA fill pattern                      */
                RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                RamAddress = RISC_RAM_START;

                while (RamLength > INIT_RAM_LENGTH)
                {
                    InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                    RamAddress += INIT_RAM_LENGTH;
                    RamLength  -= INIT_RAM_LENGTH;
                }
                InitRiscRam(RamAddress, RamLength, Value, HostAdapter);
                Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;
                Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                Checksum = 0 - Checksum;
                Value = RISC_RAM_SIZE-RISC_RAM_START;
                RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);
                if (!VerifyRiscFwChecksum(HostAdapter))
                   return (FB_RAM_FAIL);

FCDEBUGPRINT(("Completed basic RAM test\n"));
                /* now test for addressing errors                                                       */
                /* effectively write a value to each address bit and then make sure no overwrites occur */

                RamAddress = RISC_RAM_START;
                Value = 0;
                RISC_Access(RamAddress, RISC_WRITE, &Value, HostAdapter);
                Value = 1;
                for (i = 0; i < 16; i++)
                {
                    RISC_Access(RamAddress+Value, RISC_WRITE, &Value, HostAdapter);
                    Value = Value << 1;
                }
                RISC_Access(RamAddress, RISC_READ, &ReadValue, HostAdapter);
                if (ReadValue != 0)
                    return (FB_RAM_FAIL);
                Value = 1;
                for (i = 0; i < 16; i++)
                {
                    RISC_Access(RamAddress+Value, RISC_READ, &ReadValue, HostAdapter);
                    if (ReadValue != Value)
                        return (FB_RAM_FAIL);
                    Value = Value << 1;
                }
                if (FPMHWRev >= 0x06) /* then ISP2200A - we ship all 2200As with two RAMs */
                {
                    /* now test second bank of RAM*/

                    PAUSE_RISCfn(HostAdapterAddress);     /* pause for access to risc registers       */
                    Counter = 0;
#ifndef ISP24XX
                    while (!(FCRead16(HostAdapterAddress + IspHccr) & HCTLRPAUSED) && (Counter++ < 100000));/* wait for pause verification */
#else
                    while (!(FCRead32(HostAdapterAddress + IspHccr) & HCTLRPAUSED) && (Counter++ < 100000));/* wait for pause verification */
#endif
                    FCWrite16(HostAdapterAddress + 0xB0, 0xF2);                 /* select RAM Bank 2                        */
                    RELEASE_RISCfn(HostAdapterAddress);       /* Allow boot firmware to run again         */
                    Value = 0;                                                  /* test 0 fill pattern                      */

                    Value      = 0x5555;                                                  /* test 5555 fill pattern                      */
                    RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                    RamAddress = RISC_RAM_START;

                    while (RamLength > INIT_RAM_LENGTH)                                  /* can only set INIT_RAM_LENGTH words at a time*/
                    {
                        InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                        RamAddress += INIT_RAM_LENGTH;
                        RamLength  -= INIT_RAM_LENGTH;
                    }
                    InitRiscRam(RamAddress, RamLength, Value, HostAdapter);  /* fake out checksum routine to think real code     */
                    Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;          /* is present                                       */
                    Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                    Checksum = 0 - Checksum;
                    Value = RISC_RAM_SIZE-RISC_RAM_START;
                    RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                    RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);
                    if (!VerifyRiscFwChecksum(HostAdapter))
                       return (FB_RAM_FAIL);

                    Value      = 0xAAAA;                                                  /* test AAAA fill pattern                      */
                    RamLength  = RISC_RAM_SIZE - RISC_RAM_START;
                    RamAddress = RISC_RAM_START;

                    while (RamLength > INIT_RAM_LENGTH)
                    {
                        InitRiscRam(RamAddress, INIT_RAM_LENGTH, Value, HostAdapter);
                        RamAddress += INIT_RAM_LENGTH;
                        RamLength  -= INIT_RAM_LENGTH;
                    }
                    InitRiscRam(RamAddress, RamLength, Value, HostAdapter);
                    Checksum = (RISC_RAM_SIZE-RISC_RAM_START - 2) * Value;
                    Checksum += RISC_RAM_SIZE-RISC_RAM_START;
                    Checksum = 0 - Checksum;
                    Value = RISC_RAM_SIZE-RISC_RAM_START;
                    RISC_Access(RISC_RAM_START+3, RISC_WRITE, &Value, HostAdapter);
                    RISC_Access(RISC_RAM_SIZE-1, RISC_WRITE, &Checksum, HostAdapter);
                    if (!VerifyRiscFwChecksum(HostAdapter))
                       return (FB_RAM_FAIL);


                    /* now test for addressing errors                                                       */
                    /* effectively write a value to each address bit and then make sure no overwrites occur */

                    RamAddress = RISC_RAM_START;
                    Value = 0;
                    RISC_Access(RamAddress, RISC_WRITE, &Value, HostAdapter);
                    Value = 1;
                    for (i = 0; i < 16; i++)
                    {
                        RISC_Access(RamAddress+Value, RISC_WRITE, &Value, HostAdapter);
                        Value = Value << 1;
                    }
                    RISC_Access(RamAddress, RISC_READ, &ReadValue, HostAdapter);
                    if (ReadValue != 0)
                        return (FB_RAM_FAIL);
                    Value = 1;
                    for (i = 0; i < 16; i++)
                    {
                        RISC_Access(RamAddress+Value, RISC_READ, &ReadValue, HostAdapter);
                        if (ReadValue != Value)
                            return (FB_RAM_FAIL);
                        Value = Value << 1;
                    }
                }
            }
        }
        else /* 2300 or higher device and RamSize > 0*/ /* this is a fast DMA based test and also tests more than first 128Kx16 */
        {
            u32 *Ptr = (u32*)RAMTestBuffer;
            u32 RAMAddress;
            u32 TestValue;
            u32 NumAddressLines;
            u32 Address;

            *FirstFailAddress = 0;

            TestValue = 0xAAAAAAAAL;
            for (i = 0; i < RISC_RAM_SIZE/2; i++) /* fill memory with first test pattern (32 bits) RISC_RAM_SIZE is 16 bit words */
                *Ptr++ = TestValue;
            RAMSize = RAMSize & 0xFFFF0000L;     /* make sure multiple of 64K words*/
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                LoadRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
            }
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                Ptr = (u32*)RAMTestBuffer;
                for (i = 0; i < RISC_RAM_SIZE/2; i++) /* clear memory  (32 bits) */
                    *Ptr++ = 0;
                DumpRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
                Ptr = (u32*)RAMTestBuffer + RISC_RAM_START/2;
                for (i = RISC_RAM_START; i < RISC_RAM_SIZE/2; i++) /* Test memory (32 bits at a time) */
                    if (*Ptr++ != TestValue)
                    {
                       *FirstFailAddress = RAMAddress+i;
                        return(FB_RAM_FAIL);
                    }
            }

            Ptr = (u32*)RAMTestBuffer;
            TestValue = 0x55555555L;
            for (i = 0; i < RISC_RAM_SIZE/2; i++) /* fill memory with first test pattern (32 bits) RISC_RAM_SIZE is 16 bit words */
                *Ptr++ = TestValue;
            RAMSize = RAMSize & 0xFFFF0000L;     /* make sure multiple of 64K words*/
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                LoadRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
            }
            for (RAMAddress = 0; RAMAddress < RAMSize; RAMAddress += RISC_RAM_SIZE)
            {
                Ptr = (u32*)RAMTestBuffer;
                for (i = 0; i < RISC_RAM_SIZE/2; i++) /* clear memory  (32 bits) */
                    *Ptr++ = 0;
                DumpRiscRamExtended( RAMAddress + RISC_RAM_START, RAMTestBufferPA + RISC_RAM_START,
                                     RISC_RAM_SIZE - RISC_RAM_START, HostAdapter);
                Ptr = (u32*)RAMTestBuffer + RISC_RAM_START/2;
                for (i = RISC_RAM_START; i < RISC_RAM_SIZE/2; i++) /* Test memory (32 bits at a time) */
                    if (*Ptr++ != TestValue)
                    {
                       *FirstFailAddress = RAMAddress+i;
                        return(FB_RAM_FAIL);
                    }
            }

            /* now test for addressing errors                                                       */
            /* effectively write a value to each address bit and then make sure no overwrites occur */

            for ( NumAddressLines = 0; RAMSize != 0; NumAddressLines++)  /* calculate the number of address lines going to memory */

            {
                RAMSize = RAMSize >> 1;
            }

            RamAddress = RISC_RAM_START;
            WriteRiscRamExtended(RamAddress, 0xFFFF, HostAdapter);
            Address = 1;
            for (i = 1; i < NumAddressLines; i++)
            {
                WriteRiscRamExtended(RamAddress+Address, i, HostAdapter);  /* write address line # into Memory Location */
                Address = Address << 1;
            }
            ReadValue = ReadRiscRamExtended(RamAddress, HostAdapter);
            if (ReadValue != 0xFFFF)
            {

                *FirstFailAddress = RamAddress;
                return (FB_RAM_FAIL);
            }
            Address = 1;
            for (i = 1; i < NumAddressLines; i++)
            {
                ReadValue = ReadRiscRamExtended(RamAddress+Address, HostAdapter);
                if (ReadValue != i)
                {
                    *FirstFailAddress = RamAddress+Address;
                     return (FB_RAM_FAIL);
                }
                Address = Address << 1;
            }
        }
    }
#endif /* if 0 *********************************************************************************************/
    return FB_SUCCESS;       /* got this far then everything worked  */
}

#endif /* ISP24XX */
