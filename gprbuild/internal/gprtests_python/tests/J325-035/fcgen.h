/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FCGEN.H    $Revision: 1.1 $
    Module Description:     General definitions for the FC-2100 API

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

#ifndef FCGEN_H
#define FCGEN_H

#if (defined ISP2300 || defined ISP2322)
#define FIRMWARE_START_ADDRESS 0x0800   /* Firmware starts at location 0x800                   */
#define RISC_RAM_START  0x0800
#elif defined(ISP24XX)
#define FIRMWARE_START_ADDRESS 0x00100000
#define RISC_RAM_START 0x00100000
#endif
#define RX_SEQ_FIRMWARE_START_ADDRESS 0x1c000
#define TX_SEQ_FIRMWARE_START_ADDRESS 0x1e000
#define RISC_RAM_SIZE   (64*1024)

/* configuration settings */
#define FW_INIT_OPTIONS     0x8017      /*  Extended Control Block,Fair, hard address,          */
#define FAST_POST_OPTION    0x0008      /*  Fast post                                           */
#define PORT_DB_CHG_NOTIF_OPTION    0x0100      /*  Port database chang notification event      */
#define NODE_NAME_ENABLED_OPTION	0x4000		/* maintain seperate node name and port name	*/

#define MAX_RESOURCE_ALLOCATION MaxConcurrentCommands
#define THROTTLE_COUNT      128
#define RETRY_COUNT         8
#define RETRY_DELAY         10          /* 1 sec                                                */

#define MB_CMD_TIMEOUT          10000000L       /* Loops to wait for mailbox command complete   */

#define REQ_RESP_QUEUE_NUM_ENTRIES  MaxConcurrentCommands

/* queue access inline functions                                                                */
/* returns the byte pointer to the next available request queue entry                           */
#define NextReqQEntryAddress(HostAdapter)  (HostAdapter->ReqQAddress + (HostAdapter->ReqQHeadIndex * (REQ_RESP_QUEUE_ENTRY_SIZE/ADDRESSING_SIZE)))

#define NextContReqQEntryAddress(HostAdapter, Offset)  (HostAdapter->ReqQAddress + ((HostAdapter->ReqQHeadIndex+Offset) * (REQ_RESP_QUEUE_ENTRY_SIZE/ADDRESSING_SIZE)))

/* returns the byte pointer to next response queue entry                                        */
#define NextRespQEntryAddress(HostAdapter)  (HostAdapter->RespQAddress + (HostAdapter->RespQTailIndex * (REQ_RESP_QUEUE_ENTRY_SIZE/ADDRESSING_SIZE)))


/* returns the byte pointer to the next available IP RCV Buf Queue entry                         */
#define NextIPRcvBufEntryAddress(HostAdapter)  (HostAdapter->IPRcvQAddress + (HostAdapter->IPRcvQHeadIndex * (IP_RCV_QUEUE_ENTRY_SIZE/ADDRESSING_SIZE)))


#define NUM_TRANSFER_RECORDS MaxConcurrentCommands  /* for queue entries */

/* Byte access macros.  EXTRACT_BYTE extracts a designated byte from a u32 array.                */
/* STORE_BYTE stores a byte within a designated location in a u32 array.  STORE_BYTE             */
/* clears the designated byte before loading (or'ing) the data into the byte location.           */
/* Both macros have forms to accommodate the big and little endian memory interfaces             */
/*                                                                                               */

#ifndef BIG_ENDIAN
#define EXTRACT_BYTE(ARRAY_32,INDEX)  ((ARRAY_32[(INDEX)>>2]>>(8*((INDEX)&0x03)))&0x0FF)

#define STORE_BYTE(ARRAY_32,INDEX,DATA)  {ARRAY_32[(INDEX)>>2] &= ~(0x000000FF << (8*((INDEX)&0x03)));   \
                                         ARRAY_32[(INDEX)>>2] |= (((DATA)&0x0FF)<<(8*((INDEX)&0x03)));}
#else

#define EXTRACT_BYTE(ARRAY_32,INDEX)  ((ARRAY_32[(INDEX)>>2]>>(8*(3 - ((INDEX)&0x03))))&0x0FF)

#define STORE_BYTE(ARRAY_32,INDEX,DATA)  {ARRAY_32[(INDEX)>>2] &= ~(0x000000FF << (8*(3-((INDEX)&0x03))));   \
                                         ARRAY_32[(INDEX)>>2] |= (((DATA)&0x0FF)<<(8*(3-((INDEX)&0x03))));}
#endif

#ifdef BIG_ENDIAN
#define SWAP_16(VALUE)	 (((VALUE >> 8) & 0xff)|((VALUE << 8) & 0xff00))
#endif

#define SEND_TAG            0x40000000L /* tags the transfer handle as a transmit                           */
#define FC_ENTRY_AVAIL_MASK 0x80000000L /* Prequeued entry that has not be assigned a transfer yet          */
#define XFER_CNTR_MASK      0x3fffffffL /* 30 bit mask on transfer handle                                   */

#define EEPROM_WWN_START 9              /* start of WorldWideName in EEPROM                                 */
#define EEPROM_KEY_START 216			/* Start of key in EEPROM - byte offset								*/


#define SNS_RFT_ID_SUBCMD_LENGTH16 22     /* SNS RFT_ID request subcommand length in 16 bit words             */
#define SNS_RFT_ID_CMD_LENGTH16    30     /* SNS RFT_ID request command length in 16 bit words                */
#define SNS_RFT_ID_CMD             0x217  /* SNS RFT_ID command code                                          */

#define SNS_GA_NXT_SUBCMD_LENGTH16 6      /* SNS GA_NXT request subcommand length in 16 bit words             */
#define SNS_GA_NXT_CMD_LENGTH16    14     /* SNS GA_NXT request command length in 16 bit words                */
#define SNS_GA_NXT_CMD             0x100  /* SNS GA_NXT command code                                          */

#define SNS_GNN_FT_SUBCMD_LENGTH16 6      /* SNS GNN_FT request subcommand length in 16 bit words             */
#define SNS_GNN_FT_CMD_LENGTH16    14     /* SNS GNN_FT request command length in 16 bit words                */
#define SNS_GNN_FT_CMD             0x173  /* SNS GNN_FT command code                                          */

#define SNS_GID_PT_SUBCMD_LENGTH16 6      /* SNS GID_PT request subcommand length in 16 bit words             */
#define SNS_GID_PT_CMD_LENGTH16    14     /* SNS GID_PT request command length in 16 bit words                */
#define SNS_GID_PT_CMD             0x1A1  /* SNS GID_PT command code                                          */


typedef struct      /* SNS GNN_FT Request block */
{
#ifdef STRUCT_CASE1
    u16 ResponseLength16;         /* in 16 bit words */
    u16 Reserved1;
#else
    u16 Reserved1;
    u16 ResponseLength16;         /* in 16 bit words */
#endif
    u32 ResponseBufPALow;
    u32 ResponseBufPAHigh;
#ifdef STRUCT_CASE1
    u16 SubCommandLength;
    u16 Reserved2;
#else
    u16 Reserved2;
    u16 SubCommandLength;
#endif
#ifdef STRUCT_CASE1
    u16 SubCommand;
    u16 ResponseLength32;       /* in 32 bit words */
#else
    u16 ResponseLength32;       /* in 32 bit words */
    u16 SubCommand;
#endif
    u32 Reserved3;
    u32 Protocol;
} SNS_GNN_FT_Type;

typedef struct      /* SNS GNN_PT Request block */
{
#ifdef STRUCT_CASE1
    u16 ResponseLength16;         /* in 16 bit words */
    u16 Reserved1;
#else
    u16 Reserved1;
    u16 ResponseLength16;         /* in 16 bit words */
#endif
    u32 ResponseBufPALow;
    u32 ResponseBufPAHigh;
#ifdef STRUCT_CASE1
    u16 SubCommandLength;
    u16 Reserved2;
#else
    u16 Reserved2;
    u16 SubCommandLength;
#endif
#ifdef STRUCT_CASE1
    u16 SubCommand;
    u16 ResponseLength32;       /* in 32 bit words */
#else
    u16 ResponseLength32;       /* in 32 bit words */
    u16 SubCommand;
#endif
    u32 Reserved3;
    u32 PortType;
} SNS_GID_PT_Type;


typedef struct      /* SNS RFT_ID Request block */
{
#ifdef STRUCT_CASE1
    u16 ResponseLength16;         /* in 16 bit words */
    u16 Reserved1;
#else
    u16 Reserved1;
    u16 ResponseLength16;         /* in 16 bit words */
#endif
    u32 ResponseBufPALow;
    u32 ResponseBufPAHigh;
#ifdef STRUCT_CASE1
    u16 SubCommandLength;
    u16 Reserved2;
#else
    u16 Reserved2;
    u16 SubCommandLength;
#endif
#ifdef STRUCT_CASE1
    u16 SubCommand;
    u16 Reserved3;       /* in 32 bit words */
#else
    u16 Reserved3;       /* in 32 bit words */
    u16 SubCommand;
#endif
    u32 Reserved4;
    u32 PortID;
    u32 FC4_Type[8];
} SNS_RFT_ID_Type;

typedef struct          /* make sure any new entries are initialized in Fibre Init                          */
{
    unsigned   FastPostEnabled      : 1;   /* indicates the fast post option is enabled                     */
    unsigned   RequestPortMap       : 1;
    unsigned   RequestLoopID        : 1;
    unsigned   RequestLoopPortLogin : 1;
 	unsigned   NodeNameEnabled		: 1;
	unsigned   ReqSendDbgTrcAddr	: 1;
}FibreFlags;


typedef enum {
    NoMBCSent,
    PortMapRequestSent,
    PortAbortRequestSent,
    ForceLIPSent,
    OmniPortControlSent,
    IDAliasSent,            /* 5 */
    SNSSent,
    NextSNSSent,
    GetWWNSent,
    SetRDMABaseSent,
    SetRDMAMultipleSent,    /* 10 */
    SetRDMASubAddrSent,
    GetSubAddrListSent,
    GetSubAddrSingleSent,
    ImplicitPortSent,
    SetIPFastBufSent,       /* 15 */
    IPInitSent,
    LoopBackTestSent,
    GenericMBCSent,
    EnableWDCSent,
    GetLoopIDSent,          /* 20 */
    LoopPortLoginSent,
    AbortInitiatorSent,
    BusResetSent,
    LUNResetSent,
    GetLESBSent,            /* 25 */
	GetNameIdListSent,
	DiagnosticEchoSent,
	FirmwareHBSent,
	DriverHBSent,
	ResetLESBSent,          /* 30 */
    GetDataRateSent,
	GetLoginParamsSent,
	GetFibreErrorsSent,
	DataRateSent,
	SetCsFpgaOptsSent,
	SendDebugCmdSent,
	RegDbgRetAddrSent,
	RegDbgTrcAddrSent,
	SetTestFlagsSent,
	SetAddOptsSent,
	SetTskMngmtFlgsSent,
	InitiateCSRSent,
	GetRTCSent,
	InitiateCSUSent,
	DumpRAMSent,
	DumpRAMWordSent,
	GetTrcAddrnSizeSent,
	HostSetTOVsSent,
	GetSensorTempSent,
	GetSensorA_D_Sent
} MBCStateType;

typedef struct MBCQueueItem					/* Mailbox command queue 								*/
{
    MBCStateType 	MBCState;				/* Identifies the Mailbox command						*/
    u16     		Mailbox[22];		    /* holds mailbox values									*/
    void 			*Callback;				/* Ptr to callback function								*/
    u32             ClientID;               /* ID specified by user of mailbox command calls        */
    LESBType        *MBCUserLESBPointer;    /* pointer to users LESB buffer                         */
    NameIdListType	*MBSNameIdListPointer;  /* pointer to array of logged in ports					*/
	WorldWideNameType *WWNPtr;
    u8              NumMailboxRegs;         /* number of mailboxes used                             */
    u8              AbortingTargetID;  		/* ID of current target being aborted                	*/
    u16             ResetingLUN;       		/* ID of current LUN being reset                     	*/
    u8              MBCAlias;               /* alias ID                                             */
    LESBType        *MBCQUserLESBPointer;    /* pointer to users LESB buffer                         */
    ErrorLogType    *MBCQUserErrLogPointer;  /* pointer to users LESB buffer                         */
    LoginInfoType 	*MBCQLoginParamBuffPointer;/* pointer to users login paramter data type buffer	*/
	u16				MBCDbgDataLen;			/* length in bytes of debug data returned				*/
	u32				MBCDbgDataBuff;			/* address of data buffer								*/
	u32				MBCDbgDmpTrcBuffPtr;		
	u32				MBCDbgDmpTrcBuffBasePA;		
	u8				MBCDbgDmpTrcSize;		
	u32				MBCDbgDmpTrcCnt;		
	u32				MBCDbgDmpNumTrcWords;
	u8				MBCDbgDmpNumTrcBuffsRegd;
}MBCQueueItemType; 

typedef enum{
    MBCQueueSuccess,
    MBCQueueFull,
    MBCQueueEmpty
}MBCQueueErrorType;

typedef struct              /* Host adapter object.             Used to support multiple adpaters           */
{
    BaseAddressType         BaseAddress;
    u8                      PortID;
    Boolean                 Initialized;
    Boolean                 IPInitialized;
    WorldWideNameType       WorldWideName;
    WorldWideNameType       NodeName;
    u32                     RespQPA;                /* physical address of the Response Queue               */
    u32						RespQCnt;
    u32						StatusRspCnt;
    u32						RDMARxRspCnt;
    u32						CTIORspCnt;
    u32						ErrRspCnt;
    u32						LastErrHandle;
	u32						ErrHandle;
    u32                     ReqQPA;                 /* physical address of the Request Queue                */
    u32                     MBCmdBufferPA;          /* physical address of the mailbox command buffer       */
    u32                     MBCmdBufferQPA;         /* physical address of the base for the queued buffers  */
    u8                      *RespQAddress;          /* start of response queue                              */
    u8                      *ReqQAddress;           /* start of request queue                               */
#ifdef IP_SUPPORT
    u32                     IPRcvQAddressPA;        /* physical address of the Rcv buffer queue             */
    u8                      *IPRcvQAddress;         /* start of IP rcv buf queue                            */
    u16                     IPRcvQHeadIndex;        /* IP rcv buffer queue head pointer                     */
    u16                     IPRcvQTailIndex;
#endif
    u8                      *MBCommandBuffer;       /* start of the mailbox command buffer                  */
    u8                      *MBCommandBufferQ;      /* start of the mailbox command buffer queue            */
    u16                     RespQHeadIndex;         /* response queue head pointer (via index)              */
    u16                     RespQTailIndex;
    u16                     ReqQHeadIndex;
    u16                     ReqQTailIndex;
    u32                     TransferHandleCntr;     /* FCRM unique identifier for transfers                 */
                                                    /* 30 bit counter of 32 bit handle supported in the     */
                                                    /* 2100.  We set bit 30 via SEND_TAG to inidcate SEND   */
                                                    /* and bit 31 via                                       */
#ifdef NON_CACHED_WORKING_BUFFER
    tRequestQueueEntry      GlobalResponseIOCB;     /* Global cacheable IOCB for working buffer             */
    tRequestQueueEntry      GlobalRequestIOCB;      /* Global cacheable IOCB for working buffer             */
#endif
    transfer_queue_mgr_type TransferMgr;
    transfer_record_type    *TransferRecord;		/* holds base of transfer record queue					*/
	MBCQueueItemType    	*MBCQueueBasePtr;		/* Pointer to the Mailbox command queue					*/
	u32             		MBCQueueHead;		    /* Index of oldest active entry on the queue            */
	u32		                MBCQueueTail;		    /* Index of newest active entry on the queue            */
    u32                     MBCQueueNumEntries;     /* tracks the number of entries on the queue            */
    Boolean                 Waiting4ResetAck;       /* immediate notify cleanup flag                        */
    void (*TargetHandler)(TransferInfoType *);      /* pointer to target command handler                    */
    void (*FARPHandler)(FARPInfoType *);     		/* pointer to target command handler                    */
    void (*RDMANotification)(TransferInfoType *);   /* pointer to RDMA target notification handler          */
    void (*StatusHandler)(u8 Adapter, u32 Status, u32 Info);    /* pointer to status notification handler   */
    void (*MBCCallBackFunction)();                  /* pointer to mail box comand status handler            */
    LESBType                *MBCUserLESBPointer;    /* pointer to users LESB buffer                         */
    NameIdListType			*MBSNameIdListPointer;  /* pointer to array of logged in ports					*/
    WorldWideNameType       *WWNPtr;                /* used for get wwn for loop id function call           */
    ErrorLogType            *MBCUserErrLogPointer;  /* pointer to users LESB buffer                         */
    CLockDataType 			*MBCClockDataLogPointer;/* pointer to users Clock data type buffer				*/
    LoginInfoType 			*MBCLoginParamBuffPointer;/* pointer to users login paramter data type buffer	*/
	u16						DbgDataLen;				/* length in bytes of debug data returned				*/
	u32						DbgDataBuff;				/* address of data buffer								*/
	u32						DbgDmpTrcBuffPtr;		
	u32						DbgDmpTrcBuffBasePA;		
	u8						DbgDmpTrcSize;		
	u32						DbgDmpTrcCnt;		
	u32						DbgDmpNumTrcWords;
	u8						DbgDmpNumTrcBuffsRegd;
    u32                     MBCClientID;            /* ID specified by user of mailbox command calls        */
    u8                      FabricTargetPortLoopIDAlias; /* alias requested in fabric port login            */
    FIBRE_STATUS_TYPE       FibreStatus;            /* holds the host adapters status                       */
    u16                     FrameSize;              /* size of the scsi transfer frame in bytes             */
    u8                      TimeOut;                /* hardware timeout value for the host adapter          */
    u8                      OmniPortConfig;         /* configuration setting for the OmniPort               */
    volatile FibreFlags     Flags;                  /* collection of flags for the host adapter            */
    MBCStateType            MBCState;               /* The current state of the Mailbox command interface   */
    u32                     Options;                /* intialization options                                */
    u8                      AbortingTargetID;       /* ID of current target being aborted                   */
    u16                     ResetingLUN;            /* ID of current LUN being reset                        */
    u16                     MB0;                    /* copies of mailbox regs response from last Mailbox cmd*/
    u16                     MB1;
    u16                     MB2;
    u16                     MB3;
    u16                     MB4;
    u16                     MB5;
    u16                     MB6;
    u16                     MB7;
    u16                     MB8;
    u16                     MB9;
    u16                     EntryCounter;           /* synchronization counter for multitasking/processor/interrupts */
    u16                     SyncErr;                /* flag if above violated                                        */
	FIBRE_DEVICE_TYPE		DevType;				/* used to differentiate between 2322 and non 2322 builds */
}HostAdapterType;

typedef struct                      
{
    u8 Bytes[8];
} KeyType;

#endif /* FCGEN_H */

