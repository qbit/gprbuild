/* ********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FCSCSI.C    $Revision: 1.6 $
    Module Description:     FC-2100 SCSI Command Generator/Handler (initiator/target)


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
/* local functions */
static void Handle_Status_Entry(tStatusType0IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Handle_Enable_Lun_Entry(tEnableLunIOCB  *RespQ_Ptr,  HostAdapterType *HostAdapter);
static void Handle_Modify_Lun_Entry(tModifyLunIOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Handle_Immed_Notify_Entry(tImmedNotifyIOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Handle_Notify_Ack_Entry(tNotifyAckIOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Handle_ATIO(tATIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Handle_CTIO(tCTIOType2IOCB_r  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Handle_RDMA_Target_Completion(tRDMACTIOType2IOCB_r  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Handle_CommandCompleteError(tStatusType0IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);

static void Read_Sense(void);
/*static void Ack_Scsi_Bus_Rst(HostAdapterType *HostAdapter);*/
static void Start_New_IO(tATIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);

/*static int ChkCDBRsvdFlds(tATIOType2IOCB  *RespQ_Ptr,u8 RsvdFlds[], s16 Length);*/

static void Invalid_Request(void);
static void Invalid_Path(void);
static void Int_Detect_Err(void);
static void Cmd_Timeout(void);
static void Invalid_CDB(void);
static void Req_Cmplt(tCTIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void SCSI_Bus_Rst(void);
static void Unack_Event(tCTIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);
static void Unexpected_Bus_Free(void);
#if 0
static void Term_IO(void);
#endif
static void Tar_Bus_Seq_Failed(void);
static void Parity_Err(void);
static void Send_CTIO2(HostAdapterType *HostAdapter, TransferInfoType *TI, tCTIOType2IOCB *ReqQ_Ptr, Boolean CmpltXfer, u32 Handle, Boolean PreBuff);
/*static void CmdRsvd(tATIOType2IOCB  *RespQ_Ptr,tCTIOType2IOCB  *ReqQ_Ptr, HostAdapterType *HostAdapter);*/


/*static void Send_Marker(tStatusType0IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter);*/



/* Primary Initiator functions */

/* *******************************************************************************************
    SendSCSICommand

    Description:
        Sends a scsi SEND command to the designated port and subaddress which instructs the target
        to receive data on the specified subaddress.

    Parameters:
        u32 TransferHandle:           Unique transfer identification 16 bits of ID + d16 = Send
        TransferInfoType TI:          pointer to transfer information structure
        HostAdapterType *HostAdapter: pointer to host adapter struc
    Return Value:
        None

    Notes:

******************************************************************************************* */

void SendSCSICommand(u32 TransferHandle, TransferInfoType *TI, HostAdapterType *HostAdapter)
{

#ifdef NON_CACHED_WORKING_BUFFER
    tCommandType2IOCB  *ReqQPtr = (tCommandType2IOCB  *) &HostAdapter->GlobalRequestIOCB;
#else
    tCommandType2IOCB  *ReqQPtr = (tCommandType2IOCB  *) NextReqQEntryAddress(HostAdapter); /* get next available Request Queue entry   */
#endif

    ReqQPtr->Header.EntryType   = COMMAND_TYPE_2;

    ReqQPtr->Header.EntryCount  = 1;
    ReqQPtr->Header.SequenceNum = 0;                    /* system defined field                 */
    ReqQPtr->Header.EntryStatus = 0;                    /* filled in by ISP on return           */

    ReqQPtr->TransferHandle = TransferHandle;           /* TAG field for FCRM on status return  */
    ReqQPtr->LUN            = TI->Subaddress;
	if (0)// (HostAdapter->DevType >= FD_ISP_2322)
	    ReqQPtr->LoopID         = TI->Port;                 /* target ID                            */
	else
    	ReqQPtr->LoopID         = TI->Port<<8;                 /* target ID                            */
    ReqQPtr->Reserved0      = TI->Priority;      
    TI->SCSIError = FC_NULL;

    if (TI->BufferSize == 0)
        ReqQPtr->ControlFlags = 0;                      /* specify no data transfer             */
    else
     if (TransferHandle & SEND_TAG)
     {
        if (TI->Direction == FC_SEND_PREFERENCE)
            ReqQPtr->ControlFlags = DATAOUT | PRIORITY; /* Data will be sent by initiator       */
        else
            ReqQPtr->ControlFlags = DATAOUT | SIMPLE_QUEUE; /* Data will be sent by initiator       */
     }
    else
        ReqQPtr->ControlFlags = DATAIN  | SIMPLE_QUEUE; /* Data will be received by initiator   */

                                                        /* embedded design HW TX strobe support */
    if (TI->Direction == FC_SEND_THROTTLED)
        ReqQPtr->Header.SequenceNum = THROTTLED_KEY;

    if (TI->TransferProtocol == FC_RDMA_INITIATOR)
    {
        ReqQPtr->Header.SequenceNum = RDMA_VM1;
        ReqQPtr->DataSegs[2].Length = TI->RDMAOffset;    /* uses last 32 bits of IOCB for offset  */
    }
    else if (TI->TransferProtocol == FC_RDMA_INITIATOR_NO_TARGET_NOTIFICATION)
    {
        ReqQPtr->Header.SequenceNum = RDMA_VM1_NO_NOTIFICATION;
        ReqQPtr->DataSegs[2].Length = TI->RDMAOffset;    /* uses last 32 bits of IOCB for offset  */
    }

    else if (TI->TransferProtocol == FC_RDMA_INITIATOR_MULTICAST)    
    {
        ReqQPtr->Header.SequenceNum = RDMA_VM1_MULTI;
    }
    else if (TI->TransferProtocol == FC_RDMA_INITIATOR_MULTICAST_NO_TARGET_NOTIFICATION)    
    {
        ReqQPtr->Header.SequenceNum = RDMA_VM1_MULTI_NO_NOTIFICATION;
    }
    else if (TI->TransferProtocol == FC_RDMA_INITIATOR_SG_MULTICAST)    
    {
        ReqQPtr->Header.SequenceNum = RDMA_VM1_SG_MULTI;
    }
    else if (TI->TransferProtocol == FC_RDMA_INITIATOR_SG_MULTICAST_NO_TARGET_NOTIFICATION)    
    {
        ReqQPtr->Header.SequenceNum = RDMA_VM1_SG_MULTI_NO_NOTIFICATION;
    }
    ReqQPtr->Reserved0      = 0;
    ReqQPtr->Timeout        = TI->Timeout;              /* seconds                              */
    #ifndef FIXED_32BIT
    ReqQPtr->CDB[0] = *((u32 *)&TI->CDB[0]);
    ReqQPtr->CDB[1] = *((u32 *)&TI->CDB[4]);
    ReqQPtr->CDB[2] = *((u32 *)&TI->CDB[8]);
    ReqQPtr->CDB[3] = *((u32 *)&TI->CDB[12]);
    #else

    {
        int i;
        for (i = 0; i < SCSI_COMMAND_LENGTH; i++)
            STORE_BYTE(ReqQPtr->CDB,i,TI->CDB[i]);
    }
    #endif

    if (TI->DescriptorCount == 0)                           /* then just one buffer location         */
    {
        ReqQPtr->SegmentCount   = 1;
        ReqQPtr->TotalCount     = TI->BufferSize;
        ReqQPtr->DataSegs[0].Address = TI->BufferAddress;   /* tell adapter where data is       */
        ReqQPtr->DataSegs[0].Length  = TI->BufferSize;
        #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)ReqQPtr, CMDTYPE2CWORDS, CMDTYPE2SENSE, CMDTYPE2CDB, FC_TRUE);
        #endif


#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) ReqQPtr, sizeof(*ReqQPtr));
#endif

#ifdef NON_CACHED_WORKING_BUFFER
    *((tCommandType2IOCB  *) NextReqQEntryAddress(HostAdapter)) = *ReqQPtr; /* write working IOCB to queue  */
#endif

        Incr_ReqQ_Inindex(HostAdapter);                     /* inform adapter that a new IOCB is available      */

    }
    else    /* using a descriptor list  */
    {
		if (HostAdapter->DevType >= FD_ISP_2300)
		{
#ifdef CONT_IOCB_FOR_SG
#ifdef SGL_64_BIT_SUPPORT
		if (TI->DescriptorCount>2)	 
		{
			u16 CIOCBs;
			if ((TI->DescriptorCount-2)>5)	 
			{				
				CIOCBs = (TI->DescriptorCount-2)/5;
				if ((TI->DescriptorCount-2)%5)	   /*check for any beyond multiple of 7 - if so add another iocb */
					CIOCBs++;
			}
			else
				CIOCBs = 1;	
			/*printf("\nInPtr = %X, CIOCBs = %d, OutPtr = %X\n", FCRead16(HostAdapter->BaseAddress + IspRespQInPtr), CIOCBs, FCRead16(HostAdapter->BaseAddress + IspRespQOutPtr)); */
			if (((FCRead16(HostAdapter->BaseAddress + IspReqInPtr)) + CIOCBs) <= (FCRead16(HostAdapter->BaseAddress + IspReqQOutPtr))) /* check for queue full */
			{
				TI->Status = FTS_ERROR;
				return;
			}
			else
			{
		        u16 i;
	    		u32* DescriptorPtr = TI->DescriptorPtr;
	    		tCommandType3IOCB  *Cmd3IOCBPtr = (tCommandType3IOCB *) ReqQPtr;
				u8 NumDsrptrs = TI->DescriptorCount-2;
	    		Cmd3IOCBPtr->Header.EntryCount  = CIOCBs+1;
				Cmd3IOCBPtr->SegmentCount = TI->DescriptorCount;
				Cmd3IOCBPtr->TotalCount = TI->BufferSize;
		        Cmd3IOCBPtr->DataSegs[0].AddressLow  = *(DescriptorPtr++);   /* tell adapter where data is       */
		        Cmd3IOCBPtr->DataSegs[0].AddressHigh = *(DescriptorPtr++);   /* tell adapter where data is       */
		        Cmd3IOCBPtr->DataSegs[0].Length  	 = *(DescriptorPtr++);
		        Cmd3IOCBPtr->DataSegs[1].AddressLow  = *(DescriptorPtr++);   /* tell adapter where data is       */
		        Cmd3IOCBPtr->DataSegs[1].AddressHigh = *(DescriptorPtr++);   /* tell adapter where data is       */
		        Cmd3IOCBPtr->DataSegs[1].Length  	 = *(DescriptorPtr++);
#ifdef BIG_ENDIAN
			    BigEndConvert((u32 *)Cmd3IOCBPtr, CMDTYPE3CWORDS, CMDTYPE3SENSE, CMDTYPE3CDB, FC_TRUE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        		Fibre_Flush_Cache((u8*) Cmd3IOCBPtr, sizeof(*Cmd3IOCBPtr));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
			    *((tCommandType3IOCB  *) NextReqQEntryAddress(HostAdapter)) = *Cmd3IOCBPtr; /* write working IOCB to queue  */
#endif
				for (i=1; i<CIOCBs+1; i++)
				{
					u8 j, On;
#ifdef NON_CACHED_WORKING_BUFFER
				    tContinueType1_IOCB* ContIOCB = (tContinueType1_IOCB  *) &HostAdapter->GlobalRequestIOCB;
#else
					tContinueType1_IOCB* ContIOCB = (tContinueType1_IOCB *) NextContReqQEntryAddress(HostAdapter, i);
#endif
					ContIOCB->Header = Cmd3IOCBPtr->Header;
					ContIOCB->Header.EntryType = CONT_TYPE_1;
					if (NumDsrptrs >= 5)
					{
						On = 5;
						NumDsrptrs -=5;
					}
					else
						On = NumDsrptrs;
					for (j=0; j<On; j++)
					{
						ContIOCB->DataSegs[j].AddressLow  = *(DescriptorPtr++);   /* tell adapter where data is       */
						ContIOCB->DataSegs[j].AddressHigh = *(DescriptorPtr++);   /* tell adapter where data is       */
						ContIOCB->DataSegs[j].Length  	  = *(DescriptorPtr++);
					}
#ifdef BIG_ENDIAN
			    BigEndConvert((u32 *)ContIOCB, CNTTYPE1CWORDS, CNTTYPE1SENSE, CNTTYPE1CDB, FC_TRUE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        		Fibre_Flush_Cache((u8*) ContIOCB, sizeof(*ContIOCB));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
			    	*((tContinueType1_IOCB  *) NextContReqQEntryAddress(HostAdapter, i)) = *ContIOCB; /* write working IOCB to queue  */
#endif
				}
				Incr_ReqQ_Inindex_with_Cont(HostAdapter, CIOCBs+1);
			}	
		}
		else /* fit descriptors in command */
		{
    		tCommandType3IOCB  *Cmd3IOCBPtr = (tCommandType3IOCB *) ReqQPtr;
    		u32* DescriptorPtr = TI->DescriptorPtr;
			Cmd3IOCBPtr->SegmentCount = TI->DescriptorCount;
			Cmd3IOCBPtr->TotalCount = TI->BufferSize;
	        Cmd3IOCBPtr->DataSegs[0].AddressLow  = *(DescriptorPtr++);   /* tell adapter where data is       */
	        Cmd3IOCBPtr->DataSegs[0].AddressHigh = *(DescriptorPtr++);   /* tell adapter where data is       */
	        Cmd3IOCBPtr->DataSegs[0].Length  	 = *(DescriptorPtr++);
	        if (TI->DescriptorCount >1)
			{
		        Cmd3IOCBPtr->DataSegs[1].AddressLow  = *(DescriptorPtr++);   /* tell adapter where data is       */
		        Cmd3IOCBPtr->DataSegs[1].AddressHigh = *(DescriptorPtr++);   /* tell adapter where data is       */
		        Cmd3IOCBPtr->DataSegs[1].Length  	 = *(DescriptorPtr++);
			}
#ifdef BIG_ENDIAN
		    BigEndConvert((u32 *)Cmd3IOCBPtr, CMDTYPE3CWORDS, CMDTYPE3SENSE, CMDTYPE3CDB, FC_TRUE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
    		Fibre_Flush_Cache((u8*) Cmd3IOCBPtr, sizeof(*Cmd3IOCBPtr));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
		    *((tCommandType3IOCB  *) NextReqQEntryAddress(HostAdapter)) = *Cmd3IOCBPtr; /* write working IOCB to queue  */
#endif
        Incr_ReqQ_Inindex(HostAdapter);                     /* inform adapter that a new IOCB is available      */
		}	
#else	/* !SGL_64_BIT_SUPPORT */
		if (TI->DescriptorCount>3) /* can place first 3 DSD's in Command Type 2	*/
		{
			u16 CIOCBs;
			if ((TI->DescriptorCount-3)>7)	 
			{				
				CIOCBs = (TI->DescriptorCount-3)/7;
				if ((TI->DescriptorCount-3)%7)	   /*check for any beyond multiple of 7 - if so add another iocb */
					CIOCBs++;
			}
			else
				CIOCBs = 1;	
			/*printf("\nInPtr = %X, CIOCBs = %d, OutPtr = %X\n", FCRead16(HostAdapter->BaseAddress + IspRespQInPtr), CIOCBs, FCRead16(HostAdapter->BaseAddress + IspRespQOutPtr)); */
			if (((FCRead16(HostAdapter->BaseAddress + IspReqInPtr)) + CIOCBs) <= (FCRead16(HostAdapter->BaseAddress + IspReqQOutPtr))) /* check for queue full */
			{
				TI->Status = FTS_ERROR;
				return;
			}
			else
			{
		        u16 i;
	    		u32* DescriptorPtr = TI->DescriptorPtr;
				u8 NumDsrptrs = TI->DescriptorCount-3;
	    		tCommandType2IOCB  *Cmd2IOCBPtr = (tCommandType2IOCB *) ReqQPtr;
	    		Cmd2IOCBPtr->Header.EntryCount  = CIOCBs+1;
				Cmd2IOCBPtr->SegmentCount = TI->DescriptorCount;
				Cmd2IOCBPtr->TotalCount = TI->BufferSize;
		        Cmd2IOCBPtr->DataSegs[0].Address  = *DescriptorPtr++;   /* tell adapter where data is       */
		        Cmd2IOCBPtr->DataSegs[0].Length   = *DescriptorPtr++;
		        Cmd2IOCBPtr->DataSegs[1].Address  = *DescriptorPtr++;   /* tell adapter where data is       */
		        Cmd2IOCBPtr->DataSegs[1].Length   = *DescriptorPtr++;
		        Cmd2IOCBPtr->DataSegs[2].Address  = *DescriptorPtr++;   /* tell adapter where data is       */
		        Cmd2IOCBPtr->DataSegs[2].Length   = *DescriptorPtr++;
#ifdef BIG_ENDIAN
			    BigEndConvert((u32 *)Cmd2IOCBPtr, CMDTYPE2CWORDS, CMDTYPE2SENSE, CMDTYPE2CDB, FC_TRUE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        		Fibre_Flush_Cache((u8*) Cmd2IOCBPtr, sizeof(*Cmd2IOCBPtr));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
			    *((tCommandType2IOCB  *) NextReqQEntryAddress(HostAdapter)) = *Cmd2IOCBPtr; /* write working IOCB to queue  */
#endif
				for (i=1; i<CIOCBs+1; i++)
				{
					u8 j, On;
#ifdef NON_CACHED_WORKING_BUFFER
				    tContinueType0_2IOCB* ContIOCB = (tContinueType0_2IOCB  *) &HostAdapter->GlobalRequestIOCB;
#else
					tContinueType0_2IOCB* ContIOCB = (tContinueType0_2IOCB *) NextContReqQEntryAddress(HostAdapter, i);
#endif
					ContIOCB->Header = Cmd2IOCBPtr->Header;
					ContIOCB->Header.EntryType = CONT_TYPE;
					if (NumDsrptrs >= 7)
					{
						On = 7;
						NumDsrptrs -=7;
					}
					else
						On = NumDsrptrs;
					for (j=0; j<On; j++)
					{
						ContIOCB->DataSegs[j].Address  = *DescriptorPtr++;   /* tell adapter where data is       */
						ContIOCB->DataSegs[j].Length   = *DescriptorPtr++;
						/*printf("\n Address = %X, Length = %d", ContIOCB->DataSegs[j].Address, ContIOCB->DataSegs[j].Length);*/
					}
#ifdef BIG_ENDIAN
			    BigEndConvert((u32 *)ContIOCB, CNTTYPE0CWORDS, CNTTYPE0SENSE, CNTTYPE0CDB, FC_TRUE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        		Fibre_Flush_Cache((u8*) ContIOCB, sizeof(*ContIOCB));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
			    	*((tContinueType0_2IOCB  *) NextContReqQEntryAddress(HostAdapter, i)) = *ContIOCB; /* write working IOCB to queue  */
#endif
				}
				Incr_ReqQ_Inindex_with_Cont(HostAdapter, CIOCBs+1);
			}
		}
		else
		{
			u8 i = TI->DescriptorCount;
/*    		tCommandType2IOCB  *Cmd2IOCBPtr = (tCommandType2IOCB *) ReqQPtr; */
    		u32* DescriptorPtr = TI->DescriptorPtr;
			ReqQPtr->SegmentCount = TI->DescriptorCount;
			ReqQPtr->TotalCount = TI->BufferSize;
	        for (i=0; i<3; i++)
			{
		        ReqQPtr->DataSegs[i].Address  = *DescriptorPtr++;   /* tell adapter where data is       */
		        ReqQPtr->DataSegs[i].Length   = *DescriptorPtr++;
				/*printf("\n Address = %X, Length = %d", ReqQPtr->DataSegs[i].Address, ReqQPtr->DataSegs[i].Length);*/
	        }
#ifdef BIG_ENDIAN
		    BigEndConvert((u32 *)ReqQPtr, CMDTYPE2CWORDS, CMDTYPE2SENSE, CMDTYPE2CDB, FC_TRUE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
    		Fibre_Flush_Cache((u8*) ReqQPtr, sizeof(*ReqQPtr));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
		    *((tCommandType2IOCB  *) NextReqQEntryAddress(HostAdapter)) = *ReqQPtr; /* write working IOCB to queue  */
#endif
        Incr_ReqQ_Inindex(HostAdapter);                     /* inform adapter that a new IOCB is available      */
		}	
			
#endif
#else	/* 	!CONT_IOCB_FOR_SG */
        tCommandType4IOCB  *Cmd4IOCBPtr =(tCommandType4IOCB  *) ReqQPtr; /* all but entry type and  last 4 field the same as command type 2 */

        #ifdef BIG_ENDIAN   /* convert descriptor list to little endian */
         #ifdef SGL_64_BIT_SUPPORT
            BigEndConvert((u32 *) TI->DescriptorPtr, TI->DescriptorCount*3, FC_FALSE, FC_FALSE, FC_FALSE);  /* 64 bit descriptor */
         #else
			if ((TI->TransferProtocol < FC_RDMA_INITIATOR_MULTICAST) || (TI->TransferProtocol > FC_RDMA_INITIATOR_SG_MULTICAST_NO_TARGET_NOTIFICATION))
            BigEndConvert((u32 *) TI->DescriptorPtr, TI->DescriptorCount*2, FC_FALSE, FC_FALSE, FC_FALSE);  /* 32 bit descriptor */
			else
			{
				int i;
				u8 numMXPorts = ((TI->DescriptorCount & 0xff00)>>8);
				u8 numDscrptrs = (TI->DescriptorCount & 0x00ff);
				u32 numLWds;

				numLWds = numMXPorts + (numDscrptrs * 2) + 1; /* (port+offset=1 longword, descriptor=2 longwords, address=1 longword)*/
				BigEndConvert((u32 *) TI->DescriptorPtr, numLWds, FC_FALSE, FC_FALSE, FC_FALSE);
				for (i=1;i<numMXPorts+1;i++)
				{
/*					(TI->DescriptorPtr+i) = word_swap32(TI->DescriptorPtr+i);*/
					*((TI->DescriptorPtr)+i) = word_swap32(*((TI->DescriptorPtr)+i));
				}
			}
         #endif
        #endif

		if ((TI->TransferProtocol < FC_RDMA_INITIATOR_MULTICAST) || (TI->TransferProtocol > FC_RDMA_INITIATOR_SG_MULTICAST_NO_TARGET_NOTIFICATION))
        {
        Cmd4IOCBPtr->Header.EntryType = COMMAND_TYPE_4;
		}
		else
	    {
	    	FCDEBUGPRINT(("Sending Multicast\n"));
	        Cmd4IOCBPtr->Header.EntryType = COMMAND_TYPE_4_MULTI;
		}
        Cmd4IOCBPtr->SegmentCount   = TI->DescriptorCount;
        Cmd4IOCBPtr->TotalCount     = TI->BufferSize;
        #ifdef SGL_64_BIT_SUPPORT
           Cmd4IOCBPtr->DescriptorType = 1;                  /* 64 bit descriptor */
        #else
           Cmd4IOCBPtr->DescriptorType = 0;                  /* 32 bit descriptor */
        #endif
        Cmd4IOCBPtr->DescriptorBase32_63   = 0;
        Cmd4IOCBPtr->DescriptorAddressLow  = TI->BufferAddress;
        Cmd4IOCBPtr->DescriptorAddressHigh = 0;

        #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)ReqQPtr, CMDTYPE4CWORDS, CMDTYPE4SENSE, CMDTYPE4CDB, FC_TRUE);
        #endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) TI->DescriptorPtr, TI->DescriptorCount*12);    /* 12 bytes max per descriptor */
#endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) Cmd4IOCBPtr, sizeof(*Cmd4IOCBPtr));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
    *((tCommandType4IOCB  *) NextReqQEntryAddress(HostAdapter)) = *Cmd4IOCBPtr; /* write working IOCB to queue  */
#endif
        Incr_ReqQ_Inindex(HostAdapter);                     /* inform adapter that a new IOCB is available      */
#endif /* CONT_IOCB_FOR_SG */
	}
    }
}


/* IOCB Processing Functions, most except status IOCB are Target specific functions */


/*************************************************************************
*
*      Enable_LUN
*
*************************************************************************
*
* This function will prepare the ISP to receive target command.
*
* Input : lun number, HostAdapter pointer
*
* Output: None
*
*************************************************************************/

void Enable_LUN(u8 lun, HostAdapterType *HostAdapter)       /* build Enable LUN Entry */
{
    u16 i;
    tEnableLunIOCB  *ReqQ_Ptr;
    u32 *Temp_Ptr;            /* to clear queue entry */
    ReqQ_Ptr =(tEnableLunIOCB  *) NextReqQEntryAddress(HostAdapter);
    Temp_Ptr = (u32 *)ReqQ_Ptr;

    for(i=0;i<REQ_RESP_QUEUE_ENTRY_SIZE;i=i+4)          /* zero the entire Q entry  */
        *Temp_Ptr++ = 0;

    /* build enable lun queue entry */
    ReqQ_Ptr->Header.EntryType = ENABLE_LUN_TYPE;       /* entry type               */
    ReqQ_Ptr->Header.EntryCount = 0x01;                 /*entry count (always 1)    */
    ReqQ_Ptr->CommandCount = CMD_RESOURCE_CNT ;
    ReqQ_Ptr->ImmedNotifyCount =  IMMED_RESOURCE_CNT ;
    ReqQ_Ptr->Status=0;

    #ifdef BIG_ENDIAN
    BigEndConvert((u32 *)ReqQ_Ptr, ENABLELUNCWORDS, ENABLELUNSENSE, ENABLELUNCDB, FC_TRUE);
    #endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

    Incr_ReqQ_Inindex(HostAdapter);
}


/*************************************************************************
*
*  Process Response Queue.
*
*************************************************************************/

void ProcessResponseQueue(HostAdapterType *HostAdapter)
{
    tStatusType0IOCB *RespQ_Ptr;

#ifdef NON_CACHED_WORKING_BUFFER
    RespQ_Ptr = (tStatusType0IOCB  *) &HostAdapter->GlobalResponseIOCB;   /* then copy into cacheable structure */
    *RespQ_Ptr = *((tStatusType0IOCB  *) NextRespQEntryAddress(HostAdapter));
#else 
    /* update response  */
    RespQ_Ptr = (tStatusType0IOCB  *) NextRespQEntryAddress(HostAdapter);
#endif


#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Invalidate_Cache((u8*) RespQ_Ptr, sizeof(*RespQ_Ptr));
#endif


    switch (RespQ_Ptr->Header.EntryType)
    {
    case STATUS_TYPE:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, STATUSTYPECWORDS, STATUSTYPESENSE, STATUSTYPECDB, FC_TRUE);
            #endif
            FCDEBUGPRINT(("Got Status Type Response\n"));
            HostAdapter->StatusRspCnt++;
            Handle_Status_Entry(RespQ_Ptr, HostAdapter);
            break;
    case ENABLE_LUN_TYPE:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, ENABLELUNCWORDS, ENABLELUNSENSE, ENABLELUNCDB, FC_TRUE);
            #endif
            FCDEBUGPRINT(("Enable LUN Response\n"));
            Handle_Enable_Lun_Entry((tEnableLunIOCB  *)RespQ_Ptr, HostAdapter);
            break;
    case MODIFY_LUN_TYPE:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, MODIFYLUNCWORDS, MODIFYLUNSENSE, MODIFYLUNCDB, FC_TRUE);
            #endif
            Handle_Modify_Lun_Entry((tModifyLunIOCB  *)RespQ_Ptr, HostAdapter);
            break;
    case IMMED_NOTIFY_TYPE:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, IMMNOTIFYCWORDS, IMMNOTIFYSENSE, IMMNOTIFYCDB, FC_TRUE);
            #endif
            FCDEBUGPRINT(("Immed_Notify _Entry\n"));
            Handle_Immed_Notify_Entry((tImmedNotifyIOCB  *)RespQ_Ptr, HostAdapter);
            break;
    case NOTIFY_ACK_TYPE:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, NOTIFYACKCWORDS, NOTIFYACKSENSE, NOTIFYACKCDB, FC_TRUE);
            #endif
            FCDEBUGPRINT(("Notify _Ack_Entry\n"));
            Handle_Notify_Ack_Entry((tNotifyAckIOCB  *)RespQ_Ptr, HostAdapter);
            HostAdapter->Waiting4ResetAck = 0;
            break;
    case ATIO_TYPE_0:
    case ATIO_TYPE_2:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, ATIOTYPE2CWORDS, ATIOTYPE2SENSE, ATIOTYPE2CDB, FC_TRUE);
            #endif
            FCDEBUGPRINT(("ATIO received\n"));
            Handle_ATIO((tATIOType2IOCB  *)RespQ_Ptr, HostAdapter);
            break;
    case CTIO_TYPE_2:   /* CTIO */
    case CTIO_TYPE_3:   /* CTIO */
    case CTIO_TYPE_4:   /* CTIO */
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
            #endif
            if (RespQ_Ptr->Header.SequenceNum == RDMA_VM1)
            {
                FCDEBUGPRINT(("RDMA CTIO received\n"));
                HostAdapter->RDMARxRspCnt++;
                Handle_RDMA_Target_Completion((tRDMACTIOType2IOCB_r *)RespQ_Ptr, HostAdapter);
            }
            else
            {
                FCDEBUGPRINT(("CTIO received\n"));
            	HostAdapter->CTIORspCnt++;
                Handle_CTIO((tCTIOType2IOCB_r *)RespQ_Ptr, HostAdapter);
            }
            break;
	case  COMMAND_TYPE_2:      
	case  COMMAND_TYPE_3:      
	case  COMMAND_TYPE_4_MULTI:
	case  COMMAND_TYPE_4:      
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, 4, FC_FALSE, FC_FALSE, FC_TRUE);
            #endif
			Handle_CommandCompleteError(RespQ_Ptr, HostAdapter);
			break;
#ifdef IP_SUPPORT
    case IP_COMMAND_TYPE:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, CMDTYPEIP32CWORDS, CMDTYPEIP32SENSE, CMDTYPEIP32CDB, FC_TRUE);
            #endif
            FCDEBUGPRINT(("IP Command Complete\n"));
            Handle_IPComandComplete((tCommandTypeIP32IOCB *) RespQ_Ptr, HostAdapter);
            break;
    case IP_RECEIVE_TYPE:
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, IPRECEIVECWORDS,IPRECEIVESENSE, IPRECEIVECDB, FC_TRUE);
            #endif
            FCDEBUGPRINT(("IP Command Complete\n"));
            Handle_IPReceiveComplete((tIPReceiveIOCB *) RespQ_Ptr, HostAdapter);
            break;
	case FARP_REQ_TYPE:
	case FARP_REQ_MATCH_TYPE:
	case FARP_REPLY_TX_TYPE:
	case FARP_REPLY_RX_TYPE:
	{
			FC_FARP_EVENT_TYPE Event;
			if (RespQ_Ptr->Header.EntryType == FARP_REQ_TYPE)
				Event = FC_FARP_REQ_TX;
			else if (RespQ_Ptr->Header.EntryType == FARP_REQ_MATCH_TYPE)
				Event = FC_FARP_REQ_MATCH_RCVD;
			else if (RespQ_Ptr->Header.EntryType == FARP_REPLY_TX_TYPE)
				Event = FC_FARP_REPLY_TX;
			else if (RespQ_Ptr->Header.EntryType == FARP_REPLY_RX_TYPE)
				Event = FC_FARP_REPLY_RX_RCVD;
            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)RespQ_Ptr, FARPTYPE32CWORDS,FARPTYPE32SENSE, FARPTYPE32CDB, FC_TRUE);
            #endif
            Handle_FARP((tFARPIOCB *) RespQ_Ptr, HostAdapter, Event);
	}
            break;
#endif
    default:
            FCDEBUGPRINT (("Invalid Entry type=%x.\n",RespQ_Ptr->Header.EntryType));
            break;
    }
    /* end switch */
    Incr_RespQ_Outindex(HostAdapter);
}


static void Handle_Status_Entry(tStatusType0IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    TransferInfoType TI;      /* any changes here must be reflected in FC2100.c ServiceRiscInterrupt */

    SCSIStatusInfoType StatusInfo;

    FC_Q_ERR_TYPE XferQErr;

    int i;

    FCDEBUGPRINT(("Status IOCB Received\n"));
    if (RespQ_Ptr->TransferHandle & SEND_TAG)
        XferQErr = find_q_item(HostAdapter, FC_SEND, FC_INITIATOR, RespQ_Ptr->TransferHandle, &TI, FC_NULL);
    else
        XferQErr = find_q_item(HostAdapter, FC_RECEIVE, FC_INITIATOR, RespQ_Ptr->TransferHandle, &TI, FC_NULL);
    TI.Status = FTS_SUCCESS;
    TI.SCSIError = FC_NULL;


    /* if XferWErr we don't care(should never happen), we just won't call handler. Not much else to do.  */

    if ((RespQ_Ptr->SCSIStatus | RespQ_Ptr->StatusFlags | RespQ_Ptr->Status) !=0)
    {
        FCDEBUGPRINT (("Bad Transfer\n"));
        FCDEBUGPRINT (("SCSIStatus      : 0x%x\n", RespQ_Ptr->SCSIStatus));
        FCDEBUGPRINT (("StatusFlags     : 0x%x\n", RespQ_Ptr->StatusFlags));
        FCDEBUGPRINT (("Status          : 0x%x\n", RespQ_Ptr->Status));

        StatusInfo.Reserved1 = RespQ_Ptr->Status;
        StatusInfo.Reserved2 = RespQ_Ptr->StateFlags;
        StatusInfo.Reserved3 = RespQ_Ptr->StatusFlags;

        if (RespQ_Ptr->SCSIStatus & 0xFF)
        {
            switch (RespQ_Ptr->SCSIStatus & 0xFF) /* check SCSI status  */
            {
                case SCSI_CHK_COND:
                    TI.Status = FTS_CHECK_CONDITION;
                    break;
                case SCSI_STATUS_BUSY:
                    TI.Status = FTS_TARGET_BUSY;
                    break;
                case SCSI_STATUS_QUEUE_FULL:
                    TI.Status = FTS_TARGET_QUEUE_FULL;
                    break;
                case WR_PROT_ERR:
					TI.Status = FTS_WR_PROT_ERR;
					break;
                case RD_PROT_ERR:
					TI.Status = FTS_RD_PROT_ERR;
					break;
                case RANGE_ERROR:
					TI.Status = FTS_BOUNDARY_ERR;
                    break;
                default:
                    if ((RespQ_Ptr->SCSIStatus & 0xFF) >= FTS_SPECIAL_STATUS)  /* handle proprietary status */
                        TI.Status = FTS_SPECIAL_STATUS;
                    else
                        TI.Status = FTS_ERROR;
            }
        }
        else if (RespQ_Ptr->Status)
        {
#ifdef ENABLE_AMCD
			RespQ_Ptr->Status=(RespQ_Ptr->Status>>8)&0x00FF; /* shift to MSB and mask off any extra */
#endif
            switch (RespQ_Ptr->Status)
            {
                case RESET:
                case ABORT:
                    TI.Status = FTS_ABORTED;
                    break;
                case DMA:
                    TI.Status = FTS_PCI_ERROR;
                    break;
                 case TIMEOUT:
                    TI.Status = FTS_TIMEOUT;
                    break;
                case DATAOVERRUN:
                    TI.Status = FTS_OVERRUN;
                    break;
                case DATAUNDERRUN:
                    if (RespQ_Ptr->SCSIStatus & RESIDUAL_UNDER)
                        TI.Status = FTS_UNDERRUN;               /* means target meant to send less data     */
                    else
                        TI.Status = FTS_INITIATOR_UNDERRUN;    /* means initiator got less than target sent */
                    break;
                case QUEUE_FULL:
                    TI.Status = FTS_TARGET_QUEUE_FULL;
                    break;
                case PORT_NOT_AVAIL:
                case PORT_LOGD_OUT:
                case PORT_CHANGE:
                    TI.Status = FTS_INVALID_PORT;
                    break;
#ifdef ENABLE_AMCD
				case E_D_TOV:
					TI.Status = FTS_E_D_TOV;
					break;
                case ACK_TOV:
					TI.Status = FTS_ACK_TOV;
					break;
                case BB_CREDIT_E_D_TOV:
					TI.Status = FTS_BB_CREDIT_E_D_TOV;
					break;
                case SEQ_CNT_ERR:
					TI.Status = FTS_SEQ_CNT;
					break;
                case RJT_RCVD:
					TI.Status = FTS_RJT_RCVD;
					break;
                case BSY_RCVD:
					TI.Status = FTS_BSY_RCVD;
					break;
                case ABTS_RCVD:
					TI.Status = FTS_ABTS_RCVD;
					break;
                case LOSS_SYNC:
					TI.Status = FTS_LOSS_SYNC;
					break;
                case AL_TIME:
					TI.Status = FTS_AL_TIME;
					break;
                case LP_TOV:
					TI.Status = FTS_LP_TOV;
					break;
                case LP_BB_CREDIT:
					TI.Status = FTS_LP_BB_CREDIT_TOV;
					break;
#endif
                case WR_PROT_ERR:
					TI.Status = FTS_WR_PROT_ERR;
					break;
                case RD_PROT_ERR:
					TI.Status = FTS_RD_PROT_ERR;
					break;
                case RANGE_ERROR:
					TI.Status = FTS_BOUNDARY_ERR;
                    break;
                default:
                    TI.Status = FTS_ERROR;
            }
        }
        else if (RespQ_Ptr->StatusFlags)
        {
            switch(RespQ_Ptr->StatusFlags)
            {
                case ABORT_FLG:
                    TI.Status = FTS_ABORTED;
                    break;
                case TIMEOUT_FLG:
                    TI.Status = FTS_TIMEOUT;
                    break;
                default:
                    TI.Status = FTS_ERROR;
            }
        }
        StatusInfo.SCSIStatus         = RespQ_Ptr->SCSIStatus;
        StatusInfo.ResponseInfoLength = RespQ_Ptr->ResponseInfoLength;
        StatusInfo.SenseDataLength    = RespQ_Ptr->SenseDataLength;
        if (StatusInfo.SenseDataLength > SCSI_SENSE_DATA_SIZE)          /* limit reported length */
            StatusInfo.SenseDataLength = SCSI_SENSE_DATA_SIZE;
        StatusInfo.ResidualLength     = RespQ_Ptr->ResidualLength;

        for (i = 0; i < FCP_RESPONSE_INFO_SIZE; i++)
            StatusInfo.FCPResponseInfo[i] = EXTRACT_BYTE(RespQ_Ptr->FCPResponseInfo4,i);
        for (i = 0; i < SCSI_SENSE_DATA_SIZE; i++)
            StatusInfo.SCSISenseData[i]   = EXTRACT_BYTE(RespQ_Ptr->SCSISenseData,i);
        TI.SCSIError = &StatusInfo;                    /* pass all SCSI FCP status info to application */
        if (!(RespQ_Ptr->SCSIStatus & SENSE_LENGTH_VALID))
           StatusInfo.SenseDataLength = 0;                      /* if not valid then zero */
        if (!(RespQ_Ptr->SCSIStatus & FCP_RESP_INFO_LENGTH_VALID))
           StatusInfo.ResponseInfoLength = 0;                   /* if not valid then zero */
        if (!(RespQ_Ptr->Status == DATAOVERRUN || RespQ_Ptr->Status == DATAUNDERRUN))
           StatusInfo.ResidualLength = 0;                       /* if not valid then zero */
    }
    else
        TI.Status = FTS_SUCCESS;

    if ((XferQErr == FC_Q_SUCCESS)  && (TI.NotificationMethod != FC_NULL)) /* if we have a good record then notify app     */
        TI.NotificationMethod(&TI);
}

static void Handle_Enable_Lun_Entry(tEnableLunIOCB *RespQ_Ptr, HostAdapterType *HostAdapter)
{

    if(RespQ_Ptr->Status != REQUEST_CMPLT)
    {
        FCDEBUGPRINT((" ENABLE LUN ENTRY FAILED, STATUS = %x\n",RespQ_Ptr->Status));
    }
    else
    {
        FCDEBUGPRINT(("LUNs  enabled.\n"));
    }
}

static void Handle_Modify_Lun_Entry(tModifyLunIOCB *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    if (RespQ_Ptr->Status == REQUEST_CMPLT)
        return;                             /*  cmd completed OK */
    else
    {
        FCDEBUGPRINT((" Invalid status for MODIFY_LUN_IOCB\n"));
    }
}

static void Handle_Immed_Notify_Entry(tImmedNotifyIOCB *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    u16 i;
    tNotifyAckIOCB  *ReqQ_Ptr;
    u32 *Temp_Ptr;                                  /* to clear queue entry     */

    ReqQ_Ptr =(tNotifyAckIOCB  *) NextReqQEntryAddress(HostAdapter);
    Temp_Ptr = (u32 *)ReqQ_Ptr;

    for(i=0;i<REQ_RESP_QUEUE_ENTRY_SIZE;i=i+4)      /* zero the entire Q entry  */
        *Temp_Ptr++ = 0;

    FCDEBUGPRINT(("Task Flags = %X\n",RespQ_Ptr->TaskFlags));
    FCDEBUGPRINT(("Status = %X\n",RespQ_Ptr->Status));

    if (HostAdapter->StatusHandler)
        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_IMMED_NOTIFY_IOCB_RCVD, ((u32)((RespQ_Ptr->TaskFlags)<<16))|((u32)(RespQ_Ptr->Status)));

    /* build notify acknowledge queue entry */
    ReqQ_Ptr->Header.EntryType = NOTIFY_ACK_TYPE;   /* entry type               */
    ReqQ_Ptr->Header.EntryCount= 0x01;              /* entry count (always 1)   */
    ReqQ_Ptr->InitiatorID = RespQ_Ptr->InitiatorID;
    ReqQ_Ptr->RX_ID       = RespQ_Ptr->RX_ID;
    ReqQ_Ptr->TaskFlags   = RespQ_Ptr->TaskFlags;
    ReqQ_Ptr->Flags       = CMD_RES_INC;
    ReqQ_Ptr->Status      = RespQ_Ptr->Status;

    #ifdef BIG_ENDIAN
    BigEndConvert((u32 *)ReqQ_Ptr, NOTIFYACKCWORDS, NOTIFYACKSENSE, NOTIFYACKCDB, FC_TRUE);
    #endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

    Incr_ReqQ_Inindex(HostAdapter);
}

static void Handle_Notify_Ack_Entry(tNotifyAckIOCB  *RespQ_Ptr, HostAdapterType *HostAdpater)
{
    FCDEBUGPRINT(("Status = %X\n",RespQ_Ptr->Status));
}


static void Handle_ATIO(tATIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter)
{

#if 0
    if(HostAdapter->Waiting4ResetAck)                   /* this flag is set when target receives    */
    {
        /* reset. do not start a new cmd just incr */
        Req_Cmplt((tCTIOType2IOCB  *)RespQ_Ptr, HostAdapter); /* resource count.                          */
    }
    else
#endif
    {
        switch ( RespQ_Ptr->Status)
        {
            case SENSE_VALID:                               /* Autosense valid                          */
                FCDEBUGPRINT(("Handle_ATIO : Status -> SENSE_VALID\n"));
                Read_Sense();
                break;
            case BUS_DEV_RST:                               /* Bus device reset msg recived             */
                FCDEBUGPRINT(("Handle_ATIO : Status -> BUS_DEV_RST\n"));
                Req_Cmplt((tCTIOType2IOCB  *)RespQ_Ptr, HostAdapter);
                break;
            case CDB_RECEIVED:                              /* CDB received (new I/O)                   */
                FCDEBUGPRINT(("Handle_ATIO : Status -> CDB_RECEIVED\n"));
                Start_New_IO(RespQ_Ptr, HostAdapter);
                break;
            case TIME_OUT:                                  /* Time out                                 */
                FCDEBUGPRINT(("Handle_ATIO : Status -> TIME_OUT\n"));
                Cmd_Timeout();
                break;
            case INVALID_CDB:                               /* Invalid CDB                              */
                FCDEBUGPRINT(("Handle_ATIO : Status -> INVALID_CDB\n"));
                Invalid_CDB();
                break;
            case INVALID_REQUEST:                           /* Invalid request for a disable Lun        */
                FCDEBUGPRINT(("Handle_ATIO : Status -> INVALID_REQUEST\n"));
                Invalid_Request();
                break;
            case SCSI_BUS_RESET:
                FCDEBUGPRINT(("Handle_ATIO : Status -> SCSI_BUD_RESET\n"));
                SCSI_Bus_Rst();
                break;
            case UNACK_EVENT:
                FCDEBUGPRINT(("Handle_ATIO : Status -> UNACK_EVENT\n"));
                /*      Unack_Event(); */
                break;
            case UNEXPECT_BF:
                FCDEBUGPRINT(("Handle_ATIO : Status -> UNEXPECT_BF\n"));
                Unexpected_Bus_Free();
                break;
            default:
                FCDEBUGPRINT(("Handle_ATIO : Status -> Unknown\n"));
                FCDEBUGPRINT(("Status = %X\n", RespQ_Ptr->Status));
                FCDEBUGPRINT(("Received ATIO with invalid status\n"));
        } /* end switch */
    }
}
/* end function */

static void Handle_CTIO(tCTIOType2IOCB_r  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    TransferInfoType TI;      /* any changes here must be reflected in FC2100.c ServiceRiscInterrupt */
    Boolean AutoComplete, Segmented;

    FC_Q_ERR_TYPE XferQErr;

	if (RespQ_Ptr->Header.SequenceNum == SKIP_QUEUE_SEARCH_ON_COMPLETION)
		return;
	else
	{
	    if (RespQ_Ptr->TransferHandle & SEND_TAG)
	        XferQErr = find_q_item(HostAdapter, FC_SEND, FC_TARGET, RespQ_Ptr->TransferHandle, &TI, FC_NULL);
	    else
	        XferQErr = find_q_item(HostAdapter, FC_RECEIVE, FC_TARGET, RespQ_Ptr->TransferHandle, &TI, FC_NULL);
		if (XferQErr != FC_Q_SUCCESS)
		{
			FCDEBUGPRINT(("Error: Item %d Not Found on Queue!!!\n", RespQ_Ptr->TransferHandle));
			return;
		}
    }
    AutoComplete = (TI.Status == FTS_COMPLETE_TRANSEFR_DEFERRED_STATUS);   /* save previous status to check for deferred completion */
	Segmented = (TI.Status == FTS_SEGMENTED_TARGET_TRANSFER);
    TI.Status = FTS_ERROR;             /* unless changed elsewhere         */

    FCDEBUGPRINT(("Status = %X\n",RespQ_Ptr->Status ));

    switch ( RespQ_Ptr->Status)
    {
        case SENSE_VALID:              /* Autosense valid                   */
            Read_Sense();
            break;
        case BUS_DEV_RST:               /* Bus device reset msg recived     */
            Req_Cmplt((tCTIOType2IOCB *)RespQ_Ptr, HostAdapter);
            TI.Status = FTS_TARGET_RESET;
            break;
        case TIME_OUT:                  /* Time out                         */
            Cmd_Timeout();
            TI.Status = FTS_TIMEOUT;
            break;
        case INT_DETECT_ERROR:
            Int_Detect_Err();
            break;
        case INVALID_REQUEST:           /* Invalid request for a disable Lun */
            Invalid_Request();
            TI.Status = FTS_INVALID_REQUEST;
            break;
        case PCI_ERROR:
            TI.Status = FTS_PCI_ERROR;
            break;
        case INVALID_PATH:
            Invalid_Path();
            break;
        case REQUEST_ABORT:
            FCDEBUGPRINT(("Abort\n"));
            Req_Cmplt((tCTIOType2IOCB *)RespQ_Ptr, HostAdapter);
            TI.Status = FTS_ABORTED;
            break;
        case REQUEST_CMPLT:
                FCDEBUGPRINT(("!!!CMPLT CTIO received\n"));
            break;
        case SCSI_BUS_RESET:
            Req_Cmplt((tCTIOType2IOCB *)RespQ_Ptr, HostAdapter);
            /*  SCSI_Bus_Rst(); */
            TI.Status = FTS_BUS_RESET;
            break;
        case TAR_SEQ_FAILED:
            Tar_Bus_Seq_Failed();
            break;
        case UNACK_EVENT:
            Unack_Event((tCTIOType2IOCB *)RespQ_Ptr, HostAdapter);
            break;
        case PARITY_ERR:
            Parity_Err();
            break;
        case PORT_UNAVAILABLE:
        case PORT_LOGGED_OUT:
        case PORT_CHANGED:
            TI.Status = FTS_INVALID_PORT;
            break;
        case INVALID_RX_ID:
            TI.Status = FTS_INVALID_TARGET_TRANSFER;
            break;
        case CMPLT_WITH_ERROR:
            TI.Status = FTS_ERROR;
            break;
        case DATA_OVERRUN:
            TI.Status = FTS_OVERRUN;
            break;
        case LUN_RESET:
            TI.Status = FTS_LUN_RESET;
            break;
        case UNEXPECT_BF:
            Unexpected_Bus_Free();
            break;
        case RESL_TIMEOUT:
            FCDEBUGPRINT(("CTIO Reselection Time out status received %x\n", RespQ_Ptr->Status));
            Req_Cmplt((tCTIOType2IOCB *)RespQ_Ptr, HostAdapter);
            break;
        default:
            FCDEBUGPRINT(("Received CTIO with invalid status\n"));
            FCDEBUGPRINT(("Status = %X\n",RespQ_Ptr->Status ));
    }

    /* end switch */
    if (XferQErr == FC_Q_SUCCESS)
    {
        if (RespQ_Ptr->Status != REQUEST_CMPLT)
        {
            SCSIStatusInfoType Error;
            u16 i;

            FCDEBUGPRINT(("CTIO Complete Error = %x\n", RespQ_Ptr->Status));
            Error.SCSIStatus = (RespQ_Ptr->Status << 8) | (RespQ_Ptr->SCSIStatus & 0xFF);
            if (RespQ_Ptr->Status & 0x80)       /* if sense valid  */
            {
                Error.SenseDataLength = 18;

                #ifdef BIG_ENDIAN
                Error.SCSISenseData[0] = (RespQ_Ptr->Sense1 >> 8)&0xFF;
                Error.SCSISenseData[1] = (RespQ_Ptr->Sense1)&0xFF;
                #else
                Error.SCSISenseData[0] = (RespQ_Ptr->Sense1)&0xFF;
                Error.SCSISenseData[1] = (RespQ_Ptr->Sense1 >> 8)&0xFF;
                #endif
                for (i = 0; i < 16; i++)
                    Error.SCSISenseData[i+2] = EXTRACT_BYTE(RespQ_Ptr->Sense2,i);
            }
            else
                Error.SenseDataLength = 0;

            Error.ResidualLength = RespQ_Ptr->ResidualLength;
            if (RespQ_Ptr->ResidualLength > 0 && TI.Status == FTS_ERROR)
               TI.Status = FTS_UNDERRUN;
            Error.ResponseInfoLength = 0;
            TI.SCSIError = &Error;
        }
        else
        {
            TI.Status = FTS_SUCCESS;
            TI.SCSIError = FC_NULL;
        }
        if (AutoComplete || Segmented)     /* then we want to send final CTIO with status supplied by Notification Method */
        {

            tCTIOType2IOCB   *ReqQ_Ptr;

            #ifdef NON_CACHED_WORKING_BUFFER
                ReqQ_Ptr = (tCTIOType2IOCB  *) &HostAdapter->GlobalRequestIOCB;
            #else
                ReqQ_Ptr = (tCTIOType2IOCB  *) NextReqQEntryAddress(HostAdapter);
            #endif

            ReqQ_Ptr->Header.EntryType = CTIO_TYPE_2;       /* CTIO entry type          */
            ReqQ_Ptr->Header.EntryCount= 0x01;              /* entry count (always 1)   */
            ReqQ_Ptr->Header.EntryStatus = 0;               /* filled in by ISP on return           */
            ReqQ_Ptr->InitiatorID = RespQ_Ptr->InitiatorID; /* copy from ATIO           */

            ReqQ_Ptr->Timeout=HostAdapter->TimeOut;
            ReqQ_Ptr->RX_ID = RespQ_Ptr->RX_ID;             /* copy from ATIO           */
            ReqQ_Ptr->RelativeOffset = 0;
            ReqQ_Ptr->ResidualLength = 0;

			TI.RemainingTransferSize -= TI.BufferSize;
            if (Segmented && TI.RemainingTransferSize)
			{
				if (TI.NotificationMethod != FC_NULL)
					TI.NotificationMethod(&TI);
				TI.Status = FTS_SEGMENTED_TARGET_TRANSFER;
				Send_CTIO2(HostAdapter, &TI, ReqQ_Ptr, FC_FALSE, RespQ_Ptr->TransferHandle, FC_FALSE);
				if (TI.Status == FTS_TARGET_QUEUE_FULL && TI.NotificationMethod != FC_NULL)
					TI.NotificationMethod(&TI);
			}
			else
			{
	            ReqQ_Ptr->Header.SequenceNum = SKIP_QUEUE_SEARCH_ON_COMPLETION;  /* Tell completion routine to not check queue */
	            ReqQ_Ptr->Flags = RespQ_Ptr->Flags | CMD_RES_INC;

	            ReqQ_Ptr->TransferHandle= 0;                   /* since we don't queue these, we don't care about handle */                             
	            ReqQ_Ptr->TotalCount    = 0;
	            ReqQ_Ptr->SegmentCount  = 0;                                        /* data segment count   */

	            ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | SEND_SCSI_STS | NO_DATA_XFR;
	            ReqQ_Ptr->SCSIStatus = SCSI_GOOD_STATUS;                       

	            #ifdef BIG_ENDIAN
	                BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
	            #endif

	            #ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
	                Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
	            #endif

	            #ifdef NON_CACHED_WORKING_BUFFER
	                *((tCTIOType2IOCB *) NextReqQEntryAddress(HostAdapter)) = *ReqQ_Ptr; /* write working IOCB to queue  */
	            #endif
	        

	            Incr_ReqQ_Inindex(HostAdapter);
			}
        }
		else
		{ 
        	if (TI.NotificationMethod != FC_NULL) /*  then notify app     */
            	TI.NotificationMethod(&TI);
        	FCDEBUGPRINT(("Auto completion CTIO sent\n"));
		}
    }

         
}
/* end function */
static void Handle_RDMA_Target_Completion(tRDMACTIOType2IOCB_r  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    TransferInfoType TI;
	TI.Status = FTS_SUCCESS;

    FCDEBUGPRINT(("RDMA Complete IOCB\n"));
    if (RespQ_Ptr->SCSIStatus & 0xFF)
    {
        switch (RespQ_Ptr->SCSIStatus & 0xFF) /* check SCSI status  */
        {
            case SCSI_CHK_COND:
                TI.Status = FTS_CHECK_CONDITION;
                break;
            case SCSI_STATUS_BUSY:
                TI.Status = FTS_TARGET_BUSY;
                break;
            case SCSI_STATUS_QUEUE_FULL:
                TI.Status = FTS_TARGET_QUEUE_FULL;
                break;
            case WR_PROT_ERR:
				TI.Status = FTS_WR_PROT_ERR;
				break;
            case RD_PROT_ERR:
				TI.Status = FTS_RD_PROT_ERR;
				break;
            case RANGE_ERROR:
				TI.Status = FTS_BOUNDARY_ERR;
                break;
            default:
                if ((RespQ_Ptr->SCSIStatus & 0xFF) >= FTS_SPECIAL_STATUS)  /* handle proprietary status */
                    TI.Status = FTS_SPECIAL_STATUS;
                else
                    TI.Status = FTS_ERROR;
        }
    }

	if (RespQ_Ptr->Status != REQUEST_CMPLT)
    {
        switch (RespQ_Ptr->Status)
        {
            case TIME_OUT:
                TI.Status = FTS_TIMEOUT;
                break;
            case DATAOVERRUN:
                TI.Status = FTS_OVERRUN;
                break;
            case DATAUNDERRUN:
                TI.Status = FTS_UNDERRUN;
                break;
#ifdef ENABLE_AMCD
			case E_D_TOV:
				TI.Status = FTS_E_D_TOV;
				break;
            case ACK_TOV:
				TI.Status = FTS_ACK_TOV;
				break;
            case TABTS_RCVD:
				TI.Status = FTS_ABTS_RCVD;
				break;
#endif
            case WR_PROT_ERR:
				TI.Status = FTS_WR_PROT_ERR;
				break;
            case RD_PROT_ERR:
				TI.Status = FTS_RD_PROT_ERR;
				break;
            case RANGE_ERROR:
				TI.Status = FTS_BOUNDARY_ERR;
				break;
	        case PORT_UNAVAILABLE:
	        case PORT_LOGGED_OUT:
	        case PORT_CHANGED:
	            TI.Status = FTS_INVALID_PORT;
			default:
                TI.Status = FTS_ERROR;
        }
    }

    TI.TransferProtocol = FC_RDMA_TARGET;
    TI.BufferSize = RespQ_Ptr->TransferLength;
    TI.HostID     = HostAdapter->PortID;
	if (0)// (HostAdapter->DevType >= FD_ISP_2322)
    	TI.Port       = RespQ_Ptr->InitiatorID;
	else
    TI.Port       = (RespQ_Ptr->InitiatorID)>>8;
    TI.Subaddress = RespQ_Ptr->LUN;
    TI.RDMAOffset = RespQ_Ptr->RDMAOffset;
    if (RespQ_Ptr->Flags & DATA_OUT)
        TI.Direction  = FC_SEND;
    else
        TI.Direction  = FC_RECEIVE;           /* so a no data transfer will default to a receive */
    TI.BufferAddress = 0;

    TI.CDB[0] = RespQ_Ptr->CDB0;
    TI.CDB[1] = RespQ_Ptr->CDB1;
    TI.CDB[2] = RespQ_Ptr->CDB2;
    TI.CDB[3] = RespQ_Ptr->CDB3;
    TI.CDB[4] = RespQ_Ptr->CDB4;
    TI.CDB[5] = RespQ_Ptr->CDB5;
    TI.CDB[6] = RespQ_Ptr->CDB6;
    TI.CDB[7] = RespQ_Ptr->CDB7;
    TI.CDB[8] = RespQ_Ptr->CDB8;
    TI.CDB[9] = RespQ_Ptr->CDB9;
    TI.CDB[10] = RespQ_Ptr->CDB10;
    TI.CDB[11] = RespQ_Ptr->CDB11;
    TI.CDB[12] = RespQ_Ptr->CDB12;
    TI.CDB[13] = RespQ_Ptr->CDB13;
    TI.CDB[14] = RespQ_Ptr->CDB14;
    TI.CDB[15] = RespQ_Ptr->CDB15;

    if (HostAdapter->RDMANotification)        /* make sure we have a valid callback       */
        HostAdapter->RDMANotification(&TI);   /* if we have a good record then notify app */

}

static void Handle_CommandCompleteError(tStatusType0IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    TransferInfoType TI;      

    FC_Q_ERR_TYPE XferQErr;

    FCDEBUGPRINT(("Status IOCB Received\n"));
    if (RespQ_Ptr->TransferHandle & SEND_TAG)
        XferQErr = find_q_item(HostAdapter, FC_SEND, FC_INITIATOR, RespQ_Ptr->TransferHandle, &TI, FC_NULL);
    else
        XferQErr = find_q_item(HostAdapter, FC_RECEIVE, FC_INITIATOR, RespQ_Ptr->TransferHandle, &TI, FC_NULL);

    TI.Status = RespQ_Ptr->Header.EntryStatus;

    if ((XferQErr == FC_Q_SUCCESS)  && (TI.NotificationMethod != FC_NULL)) /* if we have a good record then notify app     */
        TI.NotificationMethod(&TI);
}

#if 0
static void Ack_Scsi_Bus_Rst(HostAdapterType *HostAdapter)
{
    u16 i;
    tNotifyAckIOCB  *ReqQ_Ptr;

    u32 *Temp_Ptr;            /* to clear queue entry */
    ReqQ_Ptr =(tNotifyAckIOCB  *) NextReqQEntryAddress(HostAdapter);
    Temp_Ptr = (u32 *)ReqQ_Ptr;

    for(i=0;i<REQ_RESP_QUEUE_ENTRY_SIZE;i=i+4)     /* zero the entire Q entry */
        *Temp_Ptr++ = 0;

    /* build enable lun queue entry */
    ReqQ_Ptr->Header.EntryType = NOTIFY_ACK_TYPE;     /* entry type */
    ReqQ_Ptr->Header.EntryCount= 0x01;        /* entry count (always 1) */
    ReqQ_Ptr->TaskFlags = 0x80;      /* Clear Reset */

    #ifdef BIG_ENDIAN
    BigEndConvert((u32 *)ReqQ_Ptr, NOTIFYACKCWORDS, NOTIFYACKSENSE, NOTIFYACKCDB, FC_TRUE);
    #endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

    Incr_ReqQ_Inindex(HostAdapter);
}
#endif


static void Start_New_IO(tATIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    TransferInfoType *TI;                    /* information passed to handler    */

    TransferInfoType WorkingTI;

    FC_Q_ERR_TYPE XferQErr;
    u32 Handle;
    FC_DIRECTION_TYPE Direction;

    tCTIOType2IOCB   *ReqQ_Ptr;

    u16 ValidityField = 0; 

	Boolean PreBuff = FC_FALSE;
    TI = &WorkingTI;

#ifdef NON_CACHED_WORKING_BUFFER
    ReqQ_Ptr = (tCTIOType2IOCB  *) &HostAdapter->GlobalRequestIOCB;
#else
    ReqQ_Ptr = (tCTIOType2IOCB  *) NextReqQEntryAddress(HostAdapter);
#endif

    ReqQ_Ptr->Header.EntryType = CTIO_TYPE_2;       /* CTIO entry type          */
    ReqQ_Ptr->Header.EntryCount= 0x01;              /* entry count (always 1)   */
    ReqQ_Ptr->Header.SequenceNum = 0;               /* system defined field                 */
    ReqQ_Ptr->Header.EntryStatus = 0;               /* filled in by ISP on return           */
    ReqQ_Ptr->InitiatorID = RespQ_Ptr->InitiatorID; /* copy from ATIO           */

    ReqQ_Ptr->Timeout=HostAdapter->TimeOut;
    ReqQ_Ptr->RX_ID = RespQ_Ptr->RX_ID;             /* copy from ATIO           */
    ReqQ_Ptr->TotalCount = RespQ_Ptr->DataLength;   /* copy from ATIO           */

    ReqQ_Ptr->RelativeOffset = 0;
    ReqQ_Ptr->ResidualLength = 0;

    if (HostAdapter->Flags.FastPostEnabled)
        ReqQ_Ptr->Flags = RespQ_Ptr->Flags | FAST_POST | CMD_RES_INC;
    else
        ReqQ_Ptr->Flags = RespQ_Ptr->Flags | CMD_RES_INC;
    ReqQ_Ptr->SCSIStatus =  SCSI_GOOD_STATUS;

    if (RespQ_Ptr->ExecutionCodes == 0x02) /* initiator wants data sent to it */
        Direction = FC_SEND;
    else
        Direction = FC_RECEIVE;

    TI->BufferSize = 0;
    TI->Timeout    = 0;
    TI->DescriptorCount = 0;
    TI->NotificationMethod = FC_NULL;
    TI->SCSIError = FC_NULL;

#ifndef DISABLE_PREQUEUED_SUPPORT
    /* if we can't find a buffer in target queues for this request then call handler if it exist */
    /* TI will point to the queued item if found otherwise remains pointing to WorkingTI         */
    XferQErr = target_q_item(HostAdapter, Direction, RespQ_Ptr->LUN, RespQ_Ptr->InitiatorID,  &Handle, &TI);
#else
    XferQErr = FC_Q_NOT_FOUND;
#endif

    TI->Status = FTS_SUCCESS;                        /* default                                        */

    #ifndef FIXED_32BIT                              /* everybody gets CDB - default and notification  */
    *(u32 *)&TI->CDB[0] = RespQ_Ptr->CDB[0];         /* map lwords to bytes                            */
    *(u32 *)&TI->CDB[4] = RespQ_Ptr->CDB[1];
    *(u32 *)&TI->CDB[8] = RespQ_Ptr->CDB[2];
    *(u32 *)&TI->CDB[12] =RespQ_Ptr->CDB[3];
    #else
    TI->CDB[0] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,0); /* else bytes are words so extract                */
    TI->CDB[1] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,1);
    TI->CDB[2] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,2);
    TI->CDB[3] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,3);
    TI->CDB[4] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,4);
    TI->CDB[5] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,5);
    TI->CDB[6] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,6);
    TI->CDB[7] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,7);
    TI->CDB[8] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,8);
    TI->CDB[9] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,9);
    TI->CDB[10] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,10);
    TI->CDB[11] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,11);
    TI->CDB[12] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,12);
    TI->CDB[13] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,13);
    TI->CDB[14] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,14);
    TI->CDB[15] = (u32)EXTRACT_BYTE(RespQ_Ptr->CDB,15);
    #endif

    if (XferQErr != FC_Q_SUCCESS)                   /* call the default target handler if registered  */
    {
        Handle = HostAdapter->TransferHandleCntr++ & XFER_CNTR_MASK; /* we need this either even if no default handler   */
        if (Direction < FC_RECEIVE)
            Handle = Handle | SEND_TAG;

        if (HostAdapter->TargetHandler != FC_NULL)  /* no preset buffer so see if handler has been registered   */
        {
            TI->HostID      = HostAdapter->PortID;
            TI->Direction   = Direction;
            TI->TransferProtocol = FC_TARGET;
            TI->Subaddress  = RespQ_Ptr->LUN;
			if (0)// (HostAdapter->DevType >= FD_ISP_2322)
            	TI->Port        = RespQ_Ptr->InitiatorID;
			else
            TI->Port        = RespQ_Ptr->InitiatorID>>8;
            TI->BufferSize  = RespQ_Ptr->DataLength;
            TI->RemainingTransferSize = RespQ_Ptr->DataLength;
            TI->PendedTargetHandle = Handle;
            HostAdapter->TargetHandler(TI);        /* call application's handler for unsupported target request*/
			if (TI->Timeout)
				ReqQ_Ptr->Timeout = TI->Timeout;
            /* if (TI.BufferSize < RespQ_Ptr->DataLength)*/  /* place holder */
        }
        else
            TI->Status = FTS_TARGET_BUSY;           /* no handler and no prequeue so report busy                */

		if (TI->Status != FTS_SEGMENTED_TARGET_TRANSFER || TI->Status != FTS_PEND_SEGMENTED_TARGET_TRANSFER)
		{
	        if (TI->BufferSize > RespQ_Ptr->DataLength)
	        {
	            ReqQ_Ptr->ResidualLength = TI->BufferSize - RespQ_Ptr->DataLength;
	            TI->BufferSize = RespQ_Ptr->DataLength;
	        }
	        else if (TI->BufferSize < RespQ_Ptr->DataLength)
	        {
	            ReqQ_Ptr->ResidualLength = RespQ_Ptr->DataLength - TI->BufferSize;
	            ValidityField = RESIDUAL_UNDER;
	        }
		}
    }
    else
    {
        PreBuff = FC_TRUE;
        if (TI->BufferSize > RespQ_Ptr->DataLength)
        {
            ReqQ_Ptr->ResidualLength = TI->BufferSize - RespQ_Ptr->DataLength;
            TI->BufferSize = RespQ_Ptr->DataLength;
        }
        else if (TI->BufferSize < RespQ_Ptr->DataLength)
        {
            ReqQ_Ptr->ResidualLength = RespQ_Ptr->DataLength - TI->BufferSize;
            ValidityField = RESIDUAL_UNDER;
        }

    }
    /* if transfer length > 0 then we will continue the transfer. Otherwise we will send status only indicating busy if */
    /* unable to comply or OK if CDB transfer only                                                                      */
    if ((TI->BufferSize > 0) && ((TI->Status == FTS_SUCCESS) || (TI->Status == FTS_COMPLETE_TRANSEFR_DEFERRED_STATUS) || (TI->Status == FTS_SEGMENTED_TARGET_TRANSFER)))
    {
		Boolean Cmplt = FC_TRUE;
		if (TI->Status == FTS_COMPLETE_TRANSEFR_DEFERRED_STATUS || TI->Status == FTS_SEGMENTED_TARGET_TRANSFER)
			Cmplt = FC_FALSE;
		Send_CTIO2(HostAdapter, TI, ReqQ_Ptr, Cmplt, Handle, PreBuff);
		if (TI->Status == FTS_TARGET_QUEUE_FULL && TI->NotificationMethod != FC_NULL)
			TI->NotificationMethod(TI);	
    }
    else /* no data so return busy status if either no handler, queue full or handler didn't want to handle this request*/
         /* or OK if initiator didn't want a data transfer                                                              */
    {
        if ((TI->NotificationMethod != FC_NULL)&&(insert_q_item(HostAdapter, Handle, TI, FC_NULL) != FC_Q_SUCCESS))
        {
            TI->Status =  FTS_TARGET_QUEUE_FULL;
			return;
		}
        if (TI->Status != FTS_PEND_TARGET_TRANSFER)
        {
            ReqQ_Ptr->TransferHandle= Handle;                                   /* we don't queue these */
            ReqQ_Ptr->TotalCount    = 0;
            ReqQ_Ptr->SegmentCount  = 0;                                        /* data segment count   */

            ReqQ_Ptr->DataSegs[0].Address = 0;                                  /* data segment address */
            ReqQ_Ptr->DataSegs[0].Length  = 0;                                  /* data segment length  */
            ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | SEND_SCSI_STS | NO_DATA_XFR;
            if ((RespQ_Ptr->DataLength) != 0 || (TI->Status != FTS_SUCCESS))
            {
                if (TI->Status == FTS_CHECK_CONDITION)
                    ReqQ_Ptr->SCSIStatus = SCSI_CHK_COND;                       /* specify check condition                          */
                else if (TI->Status == FTS_TARGET_BUSY)
                    ReqQ_Ptr->SCSIStatus = SCSI_BUSY_STATUS;                    /* specify busy                                     */
                else if (TI->Status == FTS_TARGET_QUEUE_FULL)
                    ReqQ_Ptr->SCSIStatus = SCSI_STATUS_QUEUE_FULL;              /* specify queue full                               */
                else if (TI->Status >= FTS_SPECIAL_STATUS)
                    ReqQ_Ptr->SCSIStatus = TI->Status;                          /* Set special code                                 */
				else
					ReqQ_Ptr->SCSIStatus = SCSI_GOOD_STATUS;					/* has to be set to something */
                HostAdapter->FibreStatus.DroppedTransferCount++;
                if (TI->SCSIError != 0)                                         /* then we have sense data to send                  */
                {
                    u32 i;
                    u8 *FCPRspSensePtr = (u8*) (&ReqQ_Ptr->TotalCount);         /* overloaded structure - See IOCB Def              */

                    #ifndef BIG_ENDIAN
                    *FCPRspSensePtr++ = FCP_RESPONSE_INFO_SIZE;
                    *FCPRspSensePtr++ = 0;
                    #else
                    *FCPRspSensePtr++ = 0;                                      /* if Big endian, swap bytes of 16 bit length       */
                    *FCPRspSensePtr++ = FCP_RESPONSE_INFO_SIZE;
                    #endif
                    ReqQ_Ptr->SenseLength = SCSI_TARGET_SENSE_DATA_SIZE;        /* user supplies FCP_RSP and FCP Sense data         */
                    for (i = 0; i < FCP_RESPONSE_INFO_SIZE; i++ )               /* copy response and sense data to be sent to initiator */
                        *FCPRspSensePtr++ = TI->SCSIError->FCPResponseInfo[i];
                    for (i = 0; i < SCSI_TARGET_SENSE_DATA_SIZE; i++ )
                        *FCPRspSensePtr++ = TI->SCSIError->SCSISenseData[i];
                    ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | STATUS_MODE1;           /* Use status mode 1 on all non good status         */
                    ReqQ_Ptr->SCSIStatus |= (3<<8);                             /* Set Response and Sense Valid                     */
                    #ifdef BIG_ENDIAN
                    BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDSBIGENDSENSEDATA, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
                    #endif

                }
                #ifdef BIG_ENDIAN
                else
                    BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
                #endif


            }
            else
            {
                ReqQ_Ptr->SCSIStatus = SCSI_GOOD_STATUS;                        /* Initiator didn't want anything anyway            */

                #ifdef BIG_ENDIAN
                BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
                #endif
            }

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
            Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

#ifdef NON_CACHED_WORKING_BUFFER
            *((tCTIOType2IOCB *) NextReqQEntryAddress(HostAdapter)) = *ReqQ_Ptr; /* write working IOCB to queue  */
#endif
            Incr_ReqQ_Inindex(HostAdapter);
        }
        /* else pended target transfer so continue and allow host to complete later */
        else
        {
#if 0
{
	u8 i;
	u32 *Ptr = (u32 *)ReqQ_Ptr;
	for (i=0;i<4;i++)
		printf("%08X %08X %08X %08X\n", *(Ptr+(i*4)), *(Ptr+1+(i*4)), *(Ptr+2+(i*4)), *(Ptr+3+(i*4))); 
}
#endif
            XferQErr = insert_q_item(HostAdapter, Handle, TI, ReqQ_Ptr);
            FCDEBUGPRINT(("Target I/O Pended\n"));
            return;
        }

    }


    FCDEBUGPRINT(("CTIO sent\n"));

}


FIBRE_TRANSFER_ERR Continue_Target_IO(TransferInfoType *TI, HostAdapterType *HostAdapter)
{

    FC_Q_ERR_TYPE       XferQErr;
    tCTIOType2IOCB      *ReqQ_Ptr;

    TransferInfoType    OriginalTI;
    
    /*u16 ValidityField = 0;*/

#ifdef NON_CACHED_WORKING_BUFFER
    ReqQ_Ptr = (tCTIOType2IOCB  *) &HostAdapter->GlobalRequestIOCB;
#else
    ReqQ_Ptr = (tCTIOType2IOCB  *) NextReqQEntryAddress(HostAdapter);
#endif

    /* Find the pended target transfer in the FCRM queue */
    XferQErr = find_q_item(HostAdapter, TI->Direction, TI->TransferProtocol,
                           TI->PendedTargetHandle, &OriginalTI, ReqQ_Ptr);

    if (XferQErr != FC_Q_SUCCESS)
        return FT_UNKNOWN_RECORD;

#if 0
{
	u8 i;
	u32 *Ptr = (u32 *)ReqQ_Ptr;
	for (i=0;i<4;i++)
		printf("%08X %08X %08X %08X\n", *(Ptr+(i*4)), *(Ptr+1+(i*4)), *(Ptr+2+(i*4)), *(Ptr+3+(i*4))); 
}
#endif
    /* should be no err since we just removed the old entry                             */
    /* if transfer length > 0 then we will continue the transfer. Otherwise we will send status only indicating busy if */
    /* unable to comply or OK if CDB transfer only                                                                      */
    /* most of Request queue already set up */
    if (TI->BufferSize > 0) /* can only be pended or segmented pended */
    {
		Boolean Cmplt = FC_TRUE;
		if (TI->Status == FTS_SEGMENTED_TARGET_TRANSFER)
			Cmplt = FC_FALSE;
		Send_CTIO2(HostAdapter, TI, ReqQ_Ptr, Cmplt, TI->PendedTargetHandle, FC_FALSE);
		if (TI->Status == FTS_TARGET_QUEUE_FULL && TI->NotificationMethod != FC_NULL)
			TI->NotificationMethod(TI);	
    }
    else /* no data so return busy status if either handler didn't want to handle this request                          */
         /* or OK if initiator didn't want a data transfer                                                              */
    {
        if ((TI->NotificationMethod != FC_NULL)&&(insert_q_item(HostAdapter, TI->PendedTargetHandle, TI, FC_NULL) != FC_Q_SUCCESS))
        {
            TI->Status =  FTS_TARGET_QUEUE_FULL;
			return FT_INTERNAL_ERROR;
		}
        ReqQ_Ptr->TransferHandle= TI->PendedTargetHandle;                   /* we don't queue these */
        ReqQ_Ptr->TotalCount    = 0;
        ReqQ_Ptr->SegmentCount  = 0;                                        /* data segment count   */

        ReqQ_Ptr->DataSegs[0].Address = 0;                                  /* data segment address */
        ReqQ_Ptr->DataSegs[0].Length  = 0;                                  /* data segment length  */
        ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | SEND_SCSI_STS | NO_DATA_XFR;
        if ((OriginalTI.BufferSize) != 0 || (TI->Status > FTS_COMPLETE_PENDED_TRANSFER))
        {
            if (TI->Status == FTS_CHECK_PENDED_TRANSFER)
                ReqQ_Ptr->SCSIStatus = SCSI_CHK_COND;                       /* specify check condition*/
            else if (TI->Status == FTS_BUSY_PENDED_TRANSFER)
                ReqQ_Ptr->SCSIStatus = SCSI_BUSY_STATUS;
            else if (TI->Status >= FTS_SPECIAL_PENDED_TRANSFER)
            	ReqQ_Ptr->SCSIStatus = TI->Status;                          /* Set special code     */
			else
				ReqQ_Ptr->SCSIStatus = SCSI_GOOD_STATUS;					/* has to be set to something */
            HostAdapter->FibreStatus.DroppedTransferCount++;
            if (TI->SCSIError != 0)                                         /* then we have sense data to send                  */
            {
                u32 i;
                u8 *FCPRspSensePtr = (u8*) (&ReqQ_Ptr->TotalCount);         /* overloaded structure - See IOCB Def              */

                #ifndef BIG_ENDIAN
                *FCPRspSensePtr++ = FCP_RESPONSE_INFO_SIZE;
                *FCPRspSensePtr++ = 0;
                #else
                *FCPRspSensePtr++ = 0;                                      /* if Big endian, swap bytes of 16 bit length       */
                *FCPRspSensePtr++ = FCP_RESPONSE_INFO_SIZE;
                #endif
                ReqQ_Ptr->SenseLength = SCSI_TARGET_SENSE_DATA_SIZE;        /* user supplies FCP_RSP and FCP Sense data         */
                for (i = 0; i < FCP_RESPONSE_INFO_SIZE; i++ )               /* copy response and sense data to be sent to initiator */
                    *FCPRspSensePtr++ = TI->SCSIError->FCPResponseInfo[i];
                for (i = 0; i < SCSI_TARGET_SENSE_DATA_SIZE; i++ )
                    *FCPRspSensePtr++ = TI->SCSIError->SCSISenseData[i];
                ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | STATUS_MODE1;           /* Use status mode 1 on all non good status         */
                ReqQ_Ptr->SCSIStatus |= (3<<8);                             /* Set Response and Sense Valid                     */
                #ifdef BIG_ENDIAN
                BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDSBIGENDSENSEDATA, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
                #endif

            }
            #ifdef BIG_ENDIAN
            else
                BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
            #endif
        }
        else
        {
            ReqQ_Ptr->SCSIStatus = SCSI_GOOD_STATUS;                        /* Initiator didn't want anything anyway */

            #ifdef BIG_ENDIAN
            BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
            #endif

        }

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

#ifdef NON_CACHED_WORKING_BUFFER
        *((tCTIOType2IOCB *) NextReqQEntryAddress(HostAdapter)) = *ReqQ_Ptr; /* write working IOCB to queue  */
#endif
        Incr_ReqQ_Inindex(HostAdapter);
    }

    FCDEBUGPRINT(("Pended CTIO sent\n"));
    return FT_SUCCESS;
}

static void Invalid_Request()
{
}
static void Invalid_Path()
{
}
static void Int_Detect_Err()
{
}
static void SCSI_Bus_Rst()
{
}
static void Unexpected_Bus_Free()
{
}
#if 0
static void Term_IO()
{
}
#endif
static void Tar_Bus_Seq_Failed()
{
}
static void Parity_Err()
{
}
static void Read_Sense()
{

}


static void Cmd_Timeout()
{
}
static void Invalid_CDB()
{
}

static void Req_Cmplt(tCTIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
#if 0
    u16 i;
    u32 *Temp_Ptr;
    tATIOType2IOCB  *ReqQ_Ptr;
    ReqQ_Ptr =(tATIOType2IOCB  *) NextReqQEntryAddress(HostAdapter);
    Temp_Ptr = (u32 *)ReqQ_Ptr;
    for(i=0;i<REQ_RESP_QUEUE_ENTRY_SIZE;i=i+4)                    /* zero the entire Q entry */
        *Temp_Ptr++ = 0;
    /* now build CTIO entry */
    ReqQ_Ptr->Header.EntryType = ATIO_TYPE_2;       /* ATIO entry type */
    ReqQ_Ptr->Header.EntryCount= 0x01;              /* entry count (always 1) */
/*    ReqQ_Ptr->LUN = RespQ_Ptr->LUN; */
    ReqQ_Ptr->Status = REQUEST_CMPLT;
    ReqQ_Ptr->RX_ID = RespQ_Ptr->RX_ID;             /* copy from CTIO */

    #ifdef BIG_ENDIAN
    BigEndConvert((u32 *)ReqQ_Ptr, ATIOTYPE2CWORDS, ATIOTYPE2SENSE, ATIOTYPE2CDB, FC_TRUE);
    #endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
    Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

    Incr_ReqQ_Inindex(HostAdapter);
#endif
}

static void Unack_Event(tCTIOType2IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
    u16 i;
    u32 *Temp_Ptr_Req;           /* ptr to ReqQ */
    u32 *Temp_Ptr_Res;           /* ptr to respQ */
    tCTIOType2IOCB  *ReqQ_Ptr;
    ReqQ_Ptr =(tCTIOType2IOCB  *) NextReqQEntryAddress(HostAdapter);
    /* copy respQ to ReqQ and chang the status */
    Temp_Ptr_Req = (u32 *)ReqQ_Ptr;
    Temp_Ptr_Res = (u32 *)RespQ_Ptr;
    for(i=0;i<REQ_RESP_QUEUE_ENTRY_SIZE;i=i+4)     /* copy the entire Q entry */
        *Temp_Ptr_Req++ =*Temp_Ptr_Res++    ;
    ReqQ_Ptr->Status=0;

    #ifdef BIG_ENDIAN
    BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
    #endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
    Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

    Incr_ReqQ_Inindex(HostAdapter);

}

static void Send_CTIO2(HostAdapterType *HostAdapter, TransferInfoType *TI, tCTIOType2IOCB *ReqQ_Ptr, Boolean CmpltXfer, u32 Handle, Boolean PreBuff)
{
	if (!(PreBuff))
	{
        if ((TI->NotificationMethod != FC_NULL)&&(insert_q_item(HostAdapter, Handle, TI, FC_NULL) != FC_Q_SUCCESS))
        {
            TI->Status =  FTS_TARGET_QUEUE_FULL;
			return;
		}
		else
			FCDEBUGPRINT(("queued Handle %d\n", Handle));
	}
    ReqQ_Ptr->TransferHandle= Handle;
    ReqQ_Ptr->TotalCount    = TI->BufferSize;
    ReqQ_Ptr->SegmentCount  = 1;                                        /* data segment count   */

    if (CmpltXfer)            /* then don't send status */
    {
        if (TI->Direction < FC_RECEIVE)
            ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | SEND_SCSI_STS | DATA_IN;
        else
            ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | SEND_SCSI_STS | DATA_OUT;
        
        if (ReqQ_Ptr->ResidualLength)
        	ReqQ_Ptr->SCSIStatus |=  RESIDUAL_UNDER | SCSI_GOOD_STATUS;
		else
        	ReqQ_Ptr->SCSIStatus |=  SCSI_GOOD_STATUS;
    }
    else
    {
        if (TI->Direction < FC_RECEIVE)
            ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | DATA_IN;
        else
            ReqQ_Ptr->Flags = ReqQ_Ptr->Flags | DATA_OUT;
    }
    if (TI->DescriptorCount == 0)                       /* then just one buffer location         */
	{
        ReqQ_Ptr->DataSegs[0].Address = TI->BufferAddress;                   /* data segment address    */
        ReqQ_Ptr->DataSegs[0].Length  = TI->BufferSize;                      /* data segment length     */
#ifdef BIG_ENDIAN
		BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE2CWORDS, CTIOTYPE2SENSE, CTIOTYPE2CDB, FC_TRUE);
#endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
	    Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
	    *((tCTIOType2IOCB *) NextReqQEntryAddress(HostAdapter)) = *ReqQ_Ptr; /* write working IOCB to queue  */
#endif

	    Incr_ReqQ_Inindex(HostAdapter);
	}
	else
    {
            tCTIOType4IOCB *CTIO4ReqQ_Ptr = (tCTIOType4IOCB *)ReqQ_Ptr;

            #ifdef BIG_ENDIAN   /* convert descriptor list to little endian */
              #ifdef SGL_64_BIT_SUPPORT
                BigEndConvert((u32 *) TI->DescriptorPtr, TI->DescriptorCount*3, FC_FALSE, FC_FALSE, FC_FALSE);  /* 64 bit descriptor */
              #else
                BigEndConvert((u32 *) TI->DescriptorPtr, TI->DescriptorCount*2, FC_FALSE, FC_FALSE, FC_FALSE);  /* 64 bit descriptor */
             #endif
            #endif

            CTIO4ReqQ_Ptr->Header.EntryType = CTIO_TYPE_4;

            CTIO4ReqQ_Ptr->SegmentCount   = TI->DescriptorCount;
            CTIO4ReqQ_Ptr->TotalCount     = TI->BufferSize;
            #ifdef SGL_64_BIT_SUPPORT
              CTIO4ReqQ_Ptr->DescriptorType = 1;          /* 64 bit descriptor */
            #else
              CTIO4ReqQ_Ptr->DescriptorType = 0;          /* 32 bit descriptor */
            #endif
            CTIO4ReqQ_Ptr->DescriptorBase32_63   = 0;
            CTIO4ReqQ_Ptr->DescriptorAddressLow  = TI->BufferAddress;
            CTIO4ReqQ_Ptr->DescriptorAddressHigh = 0;

#ifdef BIG_ENDIAN
		    BigEndConvert((u32 *)ReqQ_Ptr, CTIOTYPE4CWORDS, CTIOTYPE4SENSE, CTIOTYPE4CDB, FC_TRUE);
#endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
        	Fibre_Flush_Cache((u8*) TI->DescriptorPtr, TI->DescriptorCount*12);    /* 12 bytes max per descriptor */
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
            Fibre_Flush_Cache((u8*) CTIO4ReqQ_Ptr, sizeof(*CTIO4ReqQ_Ptr));
#endif
#ifdef NON_CACHED_WORKING_BUFFER
            *((tCTIOType4IOCB *) NextReqQEntryAddress(HostAdapter)) = *CTIO4ReqQ_Ptr; /* write working IOCB to queue  */
#endif
            Incr_ReqQ_Inindex(HostAdapter);
    }
}

#if 0
static void Send_Marker(tStatusType0IOCB  *RespQ_Ptr, HostAdapterType *HostAdapter)
{
/* ^^^^^ */

    u16 i;
    u32 *Temp_Ptr;
    tMarkerIOCB  *ReqQ_Ptr;

    ReqQ_Ptr =(tMarkerIOCB  *)& BAddr_ReqQ[ReqQ_InIndex * REQ_RESP_QUEUE_ENTRY_SIZE];
    Temp_Ptr = (u32 *)ReqQ_Ptr;
    for(i=0;i<REQ_RESP_QUEUE_ENTRY_SIZE;i=i+4)  /* zero the entire Q entry */
        *Temp_Ptr++ = 0;
    ReqQ_Ptr->Header.EntryType =MARKER_TYPE ;   /* MARKER entry type */
    ReqQ_Ptr->Header.EntryCount= 0x01;          /* entry count (always 1) */
    ReqQ_Ptr->Header.SequenceNum=RespQ_Ptr->Header.SequenceNum ;
    ReqQ_Ptr->LUN = RespQ_Ptr->LUN;
    ReqQ_Ptr->LoopID = RespQ_Ptr->TargetID;
    ReqQ_Ptr->Modifier = MKR_Mod_IT;            /* all Luns on Target n */
    #ifdef BIG_ENDIAN
    BigEndConvert((u32 *)ReqQ_Ptr, STATUSTYPECWORDS, STATUSTYPESENSE, STATUSTYPECDB, FC_TRUE);
    #endif

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
    Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif

    Incr_ReqQ_Inindex(HostAdapter);

}
#endif

