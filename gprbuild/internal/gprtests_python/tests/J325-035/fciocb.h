/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FCIOCB.H    $Revision: 1.1 $
    Module Description:     FC-2100 I/O Control Block defines


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

#include "fcapi.h"

#ifndef FCIOCB_H
#define FCIOCB_H


#define CONT_TYPE           0x02
#define STATUS_TYPE         0x03
#define MARKER_TYPE         0x04
#define EXT_CMD_TYPE        0x05
#define COMMAND_TYPE_1      0x09
#define CONT_TYPE_1         0X0A
#define COMMAND_TYPE_2      0X11
#define COMMAND_TYPE_3      0x19
#define COMMAND_TYPE_4_MULTI 0x14
#define COMMAND_TYPE_4      0x15
#define IP_COMMAND_TYPE     0x13
#define IP_RECEIVE_TYPE     0x23
/* target */
#define ENABLE_LUN_TYPE     0x0B
#define MODIFY_LUN_TYPE     0x0C
#define IMMED_NOTIFY_TYPE   0x0D
#define NOTIFY_ACK_TYPE     0x0E
#define ATIO_TYPE_0         0x06
#define CTIO_TYPE_0         0x07
#define CTIO_TYPE_1         0x0f
#define ATIO_TYPE_2         0x16
#define CTIO_TYPE_2         0x17
#define CTIO_TYPE_3         0x1f
#define CTIO_TYPE_4         0x1E
#define FARP_REQ_TYPE		0x35
#define FARP_REQ_MATCH_TYPE	0x36
#define FARP_REPLY_TX_TYPE	0x37
#define FARP_REPLY_RX_TYPE	0x38

#define CQ_FLAG_CONTINUATION 0x01
#define CQ_FLAG_FULL        0x02
#define CQ_FLAG_BADHEADER   0x04
#define CQ_FLAG_BADPACKET   0x08

/* command control flags definitions    */
#define NO_DISC         0X0001
#define HEAD_OF_QUEUE   0X0002
#define ORDERED_QUEUE   0X0004
#define SIMPLE_QUEUE    0X0008
#define NODATAXFR       0X0000
#define DATAIN          0X0020
#define DATAOUT         0X0040
#define STOP_QUEUE      0x2000
#define PRIORITY        0x8000

/* CTIO Flags definitions               */
#define STATUS_MODE0    0x0000
#define STATUS_MODE1    0x0001
#define SEND_SCSI_STS   0x8000
#define SEND_RESTOR_MSG 0x0400
#define DISABLE_SD  0x0100
#define CMD_RES_INC 0x0100
#define FAST_POST   0x0200
#define DATA_IN     0x0040
#define DATA_OUT    0x0080
#define NO_DATA_XFR     0x00c0
#define SG_LST_VALID    0x0008

/* Completion Status Error Code         */
#define COMPLETE_OK     0x0000
#define INCOMPLETE      0x0001
#define DMA             0x0002
#define TRANSPORT       0x0003
#define RESET           0x0004
#define ABORT           0x0005
#define TIMEOUT         0x0006
#define DATAOVERRUN     0x0007
#define CMDOVERRUN      0x0008
#define STSOVERRUN      0x0009
#define BADMSG          0x000A
#define NOMSGOUT        0x000B
#define EXT_ID_FAILED   0x000C
#define IDE_MSG_FAILED  0x000D
#define ABRT_MSG_FAILED 0x000E
#define REJ_MSG_FAILED  0x000F
#define NOP_MSG_FAILED  0x0010
#define PRTY_MSG_FAILED 0x0011
#define DEV_RST_FAILED  0x0012
#define ID_MSG_FAILED   0x0013
#define UNXP_BUS_FREE   0x0014
#define DATAUNDERRUN    0x0015
#define TRAN_ERR1       0x0018
#define TRAN_ERR2       0x0019
#define TRAN_ERR3       0x001A
#define QUEUE_FULL      0x001C
#define TABTS_RCVD		0x0020
#define PORT_NOT_AVAIL  0x0028
#define PORT_LOGD_OUT   0x0029
#define PORT_CHANGE     0x002A
#define WR_PROT_ERR			0x005b
#define RD_PROT_ERR			0x005c
#define RANGE_ERROR			0x005d
/* STATUS FLAG */
#define DISC_FLG        0X0001
#define SYNC_FLG        0X0002
#define PRTY_FLG        0X0004
#define BUS_RST_FLG     0X0008
#define DEV_RST_FLG     0X0010
#define ABORT_FLG       0X0020
#define TIMEOUT_FLG     0X0040
#define NEG_FLG         0X0080
#define PRLO_FLG		0x1000
#define LOGO_FLG		0x2000

/* Marker Modifier definitions      */
#define MKR_Mod_ALL 2       /* all Targets & LUNs   */
#define MKR_Mod_IT  1       /* all Luns on Target n */
#define MKR_Mod_ITL 0       /* LUN l on Target n    */


/* Enable LUN Status defintions     */
#define LUN_ALRDY_ENABlE   0x3E
#define REQUEST_CMPLT      0x01
#define REQUEST_CAPABILITY 0x16
#define INVALID_REQUEST    0x06
#define REQUEST_FAILED     0x04

/* Modify LUN Operator definitions  */
#define  NOTMODIFY_CMD_CNT   0x00
#define  ADD_TO_CMD_CNT      0x01
#define  SUB_FROM_CMD_CNT    0x02
#define  NOTMODIFY_IMMED_CNT 0x00
#define  ADD_TO_IMMED_CNT    0x04
#define  SUB_FROM_IMMED_CNT  0x80

/* Target Mode Status */
#define REQUEST_CMPLT    0x01
#define REQUEST_ABORT    0x02
#define CMPLT_WITH_ERROR 0x04 /* most likely DMA error */
#define INVALID_REQUEST  0x06
#define INVALID_PATH     0x07
#define INVALID_RX_ID    0x08
#define DATA_OVERRUN     0x09
#define RESL_TIMEOUT     0x0a
#define TIME_OUT         0x0b
#define SCSI_BUS_RESET   0x0e
#define PARITY_ERR       0x0f
#define PCI_ERROR        0x10
#define UNEXPECT_BF      0x13
#define TAR_SEQ_FAILED   0x14
#define BUS_DEV_RST      0x17
#define PORT_UNAVAILABLE 0x28
#define PORT_LOGGED_OUT  0x29
#define PORT_CHANGED     0x2A
#define INT_DETECT_ERROR 0x33
#define UNACK_EVENT      0x35
#define MSG_RCVD         0x36
#define INVALID_CDB      0x37
#define CDB_RECEIVED     0x3d
#define STATUS_MASK      0x3f
#define SRR_RECEIVED     0x45
#define LUN_RESET        0x48
#define SENSE_VALID      0x80
#define TERM_IO          0xff


/* SCSI Status field flags */
#define RESIDUAL_UNDER              FC_BIT11
#define RESIDUAL_OVER               FC_BIT10
#define SENSE_LENGTH_VALID          FC_BIT9
#define FCP_RESP_INFO_LENGTH_VALID  FC_BIT8


/* BIG Endian support definitions */
#define CBD_START_ELEMENT   5
#define CBD_BLOCK_SIZE      4
#define SENSE_FIELD_LWORD  11

#define THROTTLED_KEY        0xA5     /* code in header sequence field to indicate HW throttled transfer */
#define RDMA_VM1             0xA6     /* code to specify Low Latency Protocol - Virtual Memory version 1 */
#define RDMA_VM1_NO_NOTIFICATION 0xA7 /* must have low bit set to cance notification                     */
#define IP_FEATURE           0xA8
#define	RDMA_VM1_MULTI		0xB6
#define RDMA_VM1_MULTI_NO_NOTIFICATION  0xB7
#define	RDMA_VM1_SG_MULTI		0xF6
#define RDMA_VM1_SG_MULTI_NO_NOTIFICATION  0xF7
#define SKIP_QUEUE_SEARCH_ON_COMPLETION 0xB0

enum RiscDirectionType { RISC_READ, RISC_WRITE };

struct DataSeg
{
    u32  Address;
    u32  Length;
};

struct DataSeg64
{
    u32  AddressLow;
    u32  AddressHigh;
    u32  Length;
};

struct IOCBHeader
{
#ifndef HIGH_2_LOW
    u32 EntryType:8;
    u32 EntryCount:8;
    u32 SequenceNum:8; /* system defined field */
    u32 EntryStatus:8;
#else
    u32 EntryStatus:8;
    u32 SequenceNum:8; /* system defined field */
    u32 EntryCount:8;
    u32 EntryType:8;
#endif
};


#define CMDTYPE2CWORDS  16
#define CMDTYPE2SENSE   FC_FALSE
#define CMDTYPE2CDB     FC_TRUE

struct CommandType2IOCBStruct
{
    struct IOCBHeader   Header;
    u32 TransferHandle;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 LoopID:16;
    u32 LUN:16;
    u32 ControlFlags:16;
    u32 Reserved0:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
#else
    u32 LUN:16;
    u32 LoopID:16;
    u32 Reserved0:16;
    u32 ControlFlags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;
#endif

    u32  CDB[4];
    u32  TotalCount;
    struct  DataSeg     DataSegs[3];
};

typedef struct CommandType2IOCBStruct tCommandType2IOCB;
typedef struct CommandType2IOCBStruct tRequestQueueEntry;  /* generic request entry */



#define CMDTYPE3CWORDS  16
#define CMDTYPE3SENSE   FC_FALSE
#define CMDTYPE3CDB     FC_TRUE

struct CommandType3IOCBStruct
{
    struct IOCBHeader   Header;
    u32 TransferHandle;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 LoopID:16;
    u32 LUN:16;
    u32 ControlFlags:16;
    u32 Reserved0:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
#else
    u32 LUN:16;
    u32 LoopID:16;
    u32 Reserved0:16;
    u32 ControlFlags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;
#endif

    u32  CDB[4];
    u32  TotalCount;
    struct  DataSeg64     DataSegs[2];
};

typedef struct CommandType3IOCBStruct tCommandType3IOCB;





#define CMDTYPE4CWORDS  16
#define CMDTYPE4SENSE   FC_FALSE
#define CMDTYPE4CDB     FC_TRUE

struct CommandType4IOCBStruct
{
    struct IOCBHeader   Header;
    u32 TransferHandle;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 LoopID:16;
    u32 LUN:16;
    u32 ControlFlags:16;
    u32 Reserved0:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
#else
    u32 LUN:16;
    u32 LoopID:16;
    u32 Reserved0:16;
    u32 ControlFlags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;
#endif
    u32 CDB[SCSI_COMMAND_32_LENGTH];
    u32 TotalCount;

#ifdef STRUCT_CASE1
    u32 DescriptorType:16;
    u32 Reserved3:16;              /* delphi specific to word align the following fields */
#else
    u32 Reserved3:16;              /* delphi specific to word align the following fields */
    u32 DescriptorType:16;
#endif

    u32 DescriptorBase32_63;
    u32 DescriptorAddressLow;
    u32 DescriptorAddressHigh;
    u32 Reserved4[2];
};

typedef struct CommandType4IOCBStruct tCommandType4IOCB;



#define CNTTYPE0CWORDS  16
#define CNTTYPE0SENSE   FC_FALSE
#define CNTTYPE0CDB     FC_FALSE

struct ContinueType0_2IOCBStruct
{
    struct IOCBHeader   Header;
    u32              Token;      /* system defined field */
    struct  DataSeg     DataSegs[7];
};

typedef struct ContinueType0_2IOCBStruct tContinueType0_2IOCB;



#define CNTTYPE1CWORDS  16
#define CNTTYPE1SENSE   FC_FALSE
#define CNTTYPE1CDB     FC_FALSE

struct ContinueType1_IOCBStruct
{
    struct IOCBHeader   Header;
    struct  DataSeg64     DataSegs[5];
};

typedef struct ContinueType1_IOCBStruct tContinueType1_IOCB;



#define MARKERCWORDS  4
#define MARKERSENSE   FC_FALSE
#define MARKERCDB     FC_FALSE

struct MarkerIOCBStruct
{
    struct IOCBHeader   Header;
    u32 Token;      /* system defined field */
#ifdef STRUCT_CASE1
    u32  LoopID:16;
    u32  Modifier:8;
    u32  Reserved1:8;
    u32  Flags:16;
    u32  LUN:16;
#else
    u32  Reserved1:8;
    u32  Modifier:8;
    u32  LoopID:16;
    u32  LUN:16;
    u32  Flags:16;
    
#endif
    u32 Reserved[13];
};

typedef struct MarkerIOCBStruct tMarkerIOCB;


#define STATUSTYPECWORDS  6
#define STATUSTYPESENSE   FC_FALSE
#define STATUSTYPECDB     FC_FALSE

struct StatusType0IOCBStruct
{
    struct IOCBHeader   Header;
    u32 TransferHandle;

#ifdef STRUCT_CASE1
    u32 SCSIStatus:16;
    u32 Status:16;
    u32 StateFlags:16;
    u32 StatusFlags:16;
    u32 ResponseInfoLength:16;
    u32 SenseDataLength:16;
#else
    u32 Status:16;
    u32 SCSIStatus:16;
    u32 StatusFlags:16;
    u32 StateFlags:16;
    u32 SenseDataLength:16;
    u32 ResponseInfoLength:16;
#endif
    u32 ResidualLength;
    u32 FCPResponseInfo4[FCP_RESPONSE_INFO_32_SIZE];
    u32 SCSISenseData[SCSI_SENSE_DATA_32_SIZE];
};

typedef struct StatusType0IOCBStruct tStatusType0IOCB;
typedef struct StatusType0IOCBStruct tResponseQueueEntry; /* generic response */



#define ENABLELUNCWORDS   6
#define ENABLELUNSENSE    FC_FALSE
#define ENABLELUNCDB      FC_FALSE

struct EnableLunIOCBStruct
{
    struct IOCBHeader   Header;
    u32 Token;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 ReservedA:8;
    u32 Reserved1:8;
    u32 Reserved2:16;
    u32 Reserved3;
    u32 Status:8;
    u32 Reserved4:8;
    u32 CommandCount:8;
    u32 ImmedNotifyCount:8;
    u32 Reserved5:16;
    u32 Timeout:16;
#else
    u32 Reserved2:16;
    u32 Reserved1:8;
    u32 ReservedA:8;
    u32 Reserved3;
    u32 ImmedNotifyCount:8;
    u32 CommandCount:8;
    u32 Reserved4:8;
    u32 Status:8;
    u32 Timeout:16;
    u32 Reserved5:16;
#endif
    u32  Reserved6[10];
};

typedef struct EnableLunIOCBStruct tEnableLunIOCB;


#define MODIFYLUNCWORDS   6
#define MODIFYLUNSENSE    FC_FALSE
#define MODIFYLUNCDB      FC_FALSE

struct ModifyLunIOCBStruct
{
    struct IOCBHeader   Header;
    u32  Token;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 ReservedA:8;
    u32 Reserved1:8;
    u32 Operators:8;
    u32 Reserved2:8;
    u32 Reserved3;
    u32 Status:8;
    u32 Reserved4:8;
    u32 CommandCount:8;
    u32 ImmedNotifyCount:8;
    u32 Reserved5:16;
    u32 Timeout:16;
#else
    u32 Reserved2:8;
    u32 Operators:8;
    u32 Reserved1:8;
    u32 ReservedA:8;
    u32 Reserved3;
    u32 ImmedNotifyCount:8;
    u32 CommandCount:8;
    u32 Reserved4:8;
    u32 Status:8;
    u32 Timeout:16;
    u32 Reserved5:16;
#endif
    u32  Reserved6[10];
};

typedef struct ModifyLunIOCBStruct tModifyLunIOCB;


#define IMMNOTIFYCWORDS  6
#define IMMNOTIFYSENSE   FC_TRUE
#define IMMNOTIFYCDB     FC_FALSE

struct ImmedNotifyIOCBStruct
{
    struct IOCBHeader   Header;
    u32 Token; /* system defined field */
#ifdef STRUCT_CASE1
    u32 InitiatorID:16;
    u32 LUN:16;
    u32 Reserved2;
    u32 Status:16;
    u32 TaskFlags:16;
    u32 RX_ID:16;
    u32 Reserved3:16;
#else
    u32 LUN:16;
    u32 InitiatorID:16;
    u32 Reserved2;
    u32 TaskFlags:16;
    u32 Status:16;
    u32 Reserved3:16;
    u32 RX_ID:16;
#endif
    u32 Reserved4[5];
    u32 SCSIStatus:16;
    u32 Sense1:16;
    u32 Sense2[4];                         
};

typedef struct ImmedNotifyIOCBStruct tImmedNotifyIOCB;


#define NOTIFYACKCWORDS   6
#define NOTIFYACKSENSE    FC_FALSE
#define NOTIFYACKCDB      FC_FALSE

struct NotifyAckIOCBStruct
{
    struct IOCBHeader   Header;
    u32  Token; /* system defined field */
#ifdef STRUCT_CASE1
    u32 InitiatorID:16;
    u32 Reserved1:8;
    u32 LoopID:8;
    u32 Flags:16;
    u32 Reserved2:16;
    u32 Status:16;
    u32 TaskFlags:16;
    u32 RX_ID:16;
    u32 Reserved3:16;
#else
    u32 LoopID:8;
    u32 Reserved1:8;
    u32 InitiatorID:16;
    u32 Reserved2:16;
    u32 Flags:16;
    u32 TaskFlags:16;
    u32 Status:16;
    u32 Reserved3:16;
    u32 RX_ID:16;
#endif
    u32 Reserved4[10];
};

typedef struct NotifyAckIOCBStruct tNotifyAckIOCB;


#define ATIOTYPE2CWORDS  11
#define ATIOTYPE2SENSE   FC_TRUE
#define ATIOTYPE2CDB     FC_TRUE

struct ATIOType2IOCBStruct
{
    struct IOCBHeader   Header;
    u32 Token;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 InitiatorID:16;
    u32 RX_ID:16;
    u32 Flags:16;
    u32 Status:16;
    u32 Reserved1:8;
    u32 TaskCodes:8;
    u32 TaskFlags:8;
    u32 ExecutionCodes:8;
#else
    u32 RX_ID:16;
    u32 InitiatorID:16;
    u32 Status:16;
    u32 Flags:16;
    u32 ExecutionCodes:8;
    u32 TaskFlags:8;
    u32 TaskCodes:8;
    u32 Reserved1:8;
#endif
    u32 CDB[SCSI_COMMAND_32_LENGTH];
    u32 DataLength;
#ifdef STRUCT_CASE1
    u32 LUN:16;
    u32 Reserved2:16;
#else
    u32 Reserved2:16;
    u32 LUN:16;
#endif
    u32 SCSIStatus:16;
    u32 Sense1:16;
    u32 Sense2[4];
};

typedef struct ATIOType2IOCBStruct tATIOType2IOCB;


#define CTIOTYPE2CWORDS  16
#define CTIOTYPE2CWORDSBIGENDSENSEDATA 9  /* number of words to big endian convert if sending sense data */
#define CTIOTYPE2SENSE   FC_FALSE
#define CTIOTYPE2CDB     FC_FALSE

struct CTIOType2IOCBStruct {
    struct IOCBHeader   Header;
    u32 TransferHandle;
#ifdef STRUCT_CASE1
    u32 InitiatorID:16;
    u32 RX_ID:16;
    u32 Flags:16;
    u32 Status:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
    u32 RelativeOffset;
    u32 ResidualLength;
    u32 Reserved1;
    u32 SenseLength:16;
    u32 SCSIStatus:16;
#else
    u32 RX_ID:16;
    u32 InitiatorID:16;
    u32 Status:16;
    u32 Flags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;
    u32 RelativeOffset;
    u32 ResidualLength;
    u32 Reserved1;
    u32 SCSIStatus:16;
    u32 SenseLength:16;
#endif
    u32  TotalCount;                /* Also u16 ResponseLength;u8 FCP_RSP[8]; u8 FCP_Sense[16] starting at byte offset 0x24 */
    struct  DataSeg DataSegs[3];
};

typedef struct CTIOType2IOCBStruct tCTIOType2IOCB;

/* Target CTIO Type 2 Returned from ISP f/w  */

#define RCTIOTYPE2CWORDS   7
#define RCTIOTYPE2SENSE    FC_TRUE
#define RCTIOTYPE2CDB      FC_FALSE

struct CTIOType2IOCB_rStruct {
    struct IOCBHeader   Header;
    u32  TransferHandle;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 InitiatorID:16;
    u32 RX_ID:16;
    u32 Flags:16;
    u32 Status:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
#else
    u32 RX_ID:16;
    u32 InitiatorID:16;
    u32 Status:16;
    u32 Flags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;
#endif
    u32 RelativeOffset;
    u32 ResidualLength;
    u32 Reserved1[4];
    u32 SCSIStatus:16;
    u32 Sense1:16;
    u32 Sense2[4];
};

typedef struct CTIOType2IOCB_rStruct tCTIOType2IOCB_r;

/* RDMA Target CTIO Type 2 Returned from ISP f/w  */

#define RDMARCTIOTYPE2CWORDS   16
#define RDMARCTIOTYPE2SENSE    FC_FALSE
#define RDMARCTIOTYPE2CDB      FC_FALSE

struct RDMACTIOType2IOCB_rStruct {
    struct IOCBHeader   Header;
    u32  TransferHandle;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 InitiatorID:16;
    u32 RX_ID:16;
    u32 Flags:16;
    u32 Status:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
    u32 RelativeOffset;
    u32 ResidualLength;
    u32 LUN:16;
    u32 CDB0:8;
    u32 CDB1:8;
    u32 CDB2:8;
    u32 CDB3:8;
    u32 SCSIStatus:16;
    u32 Reserved1[2];
    u32 TransferLength;
    u32 RDMAOffset;
    
    u32 CDB4:8;
    u32 CDB5:8;
    u32 CDB6:8;
    u32 CDB7:8;

    u32 CDB8:8;
    u32 CDB9:8;
    u32 CDB10:8;
    u32 CDB11:8;
   
    u32 CDB12:8;
    u32 CDB13:8;
    u32 CDB14:8;
    u32 CDB15:8;
#else
    u32 RX_ID:16;
    u32 InitiatorID:16;
    u32 Status:16;
    u32 Flags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;

    u32 RelativeOffset;
    u32 ResidualLength;
    u32 CDB1:8;
    u32 CDB0:8;
    u32 LUN:16;
    
    u32 SCSIStatus:16;
    u32 CDB3:8;
    u32 CDB2:8;
    
    u32 Reserved1[2];
    u32 TransferLength;
    u32 RDMAOffset;
    
    u32 CDB7:8;
    u32 CDB6:8;
    u32 CDB5:8;
    u32 CDB4:8;

    u32 CDB11:8;
    u32 CDB10:8;
    u32 CDB9:8;
    u32 CDB8:8;
   
    u32 CDB15:8;
    u32 CDB14:8;
    u32 CDB13:8;
    u32 CDB12:8;
#endif    
};



typedef struct RDMACTIOType2IOCB_rStruct tRDMACTIOType2IOCB_r;




#define CTIOTYPE4CWORDS  16
#define CTIOTYPE4SENSE   FC_FALSE
#define CTIOTYPE4CDB     FC_FALSE

struct CTIOType4IOCBStruct {
    struct IOCBHeader   Header;
    u32 TransferHandle;
#ifdef STRUCT_CASE1
    u32 InitiatorID:16;
    u32 RX_ID:16;
    u32 Flags:16;
    u32 Status:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
    u32 RelativeOffset;
    u32 ResidualLength;
    u32 Reserved1;
    u32 Reserved2:16;
    u32 SCSIStatus:16;
#else
    u32 RX_ID:16;
    u32 InitiatorID:16;
    u32 Status:16;
    u32 Flags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;
    u32 RelativeOffset;
    u32 ResidualLength;
    u32 Reserved1;
    u32 SCSIStatus:16;
    u32 Reserved2:16;
#endif
    u32  TotalCount;
#ifdef STRUCT_CASE1
    u32 DescriptorType:16;
    u32 Reserved3:16;              /* delphi specific to word align the following fields */
#else
    u32 Reserved3:16;              /* delphi specific to word align the following fields */
    u32 DescriptorType:16;
#endif
    u32 DescriptorBase32_63;
    u32 DescriptorAddressLow;
    u32 DescriptorAddressHigh;
    u32 Reserved4[2];
};

typedef struct CTIOType4IOCBStruct tCTIOType4IOCB;


#ifdef IP_SUPPORT

#define CMDTYPEIP32CWORDS  16
#define CMDTYPEIP32SENSE   FC_FALSE
#define CMDTYPEIP32CDB     FC_FALSE

struct CommandTypeIP32IOCBStruct
{
    struct IOCBHeader   Header;
    u32 TransferHandle;      /* system defined field */
#ifdef STRUCT_CASE1
    u32 LoopID:16;
    u32 CompletionStatus:16;
    u32 ControlFlags:16;
    u32 Reserved0:16;
    u32 Timeout:16;
    u32 SegmentCount:16;
#else
    u32 CompletionStatus:16;
    u32 LoopID:16;
    u32 Reserved0:16;
    u32 ControlFlags:16;
    u32 SegmentCount:16;
    u32 Timeout:16;
#endif

    u32  Reserved3[4];
    u32  TotalCount;
    struct  DataSeg     DataSegs[3];
};

typedef struct CommandTypeIP32IOCBStruct tCommandTypeIP32IOCB;

struct IPBufferContainerStruct
{
#ifdef STRUCT_CASE1
    u32 ReceivePtrPALo;
    u32 ReceivePtrPAHi;
    u32 BufferHandle:16;
    u32 Reserved:16;
#else
    u32 ReceivePtrPALo;
    u32 ReceivePtrPAHi;
    u32 Reserved:16;
    u32 BufferHandle:16;
#endif
};

typedef struct IPBufferContainerStruct IPBufContainerType;


#define IPRECEIVECWORDS  16
#define IPRECEIVESENSE   FC_FALSE
#define IPRECEIVECDB     FC_FALSE

struct IPReceiveIOCBStruct
{
    struct IOCBHeader   Header;
#ifdef STRUCT_CASE1
    u32 SIDLow:16;
    u32 SIDHigh:8;
    u32 Reserved0:8;
    
    u32 LoopID:16;
    u32 CompletionFlags:16;
    
    u32 Class:16;
    u32 SequenceLength:16;
    
    u32 BufferHandle00:16;
    u32 BufferHandle01:16;
    
    u32 BufferHandle02:16;
    u32 BufferHandle03:16;
    
    u32 BufferHandle04:16;
    u32 BufferHandle05:16;
    
    u32 BufferHandle06:16;
    u32 BufferHandle07:16;
    
    u32 BufferHandle08:16;
    u32 BufferHandle09:16;
    
    u32 BufferHandle10:16;
    u32 BufferHandle11:16;
    
    u32 BufferHandle12:16;
    u32 BufferHandle13:16;
    
    u32 BufferHandle14:16;
    u32 BufferHandle15:16;
    
    u32 BufferHandle16:16;
    u32 BufferHandle17:16;
    
    u32 BufferHandle18:16;
    u32 BufferHandle19:16;
    
    u32 BufferHandle20:16;
    u32 BufferHandle21:16;
    
    u32 BufferHandle22:16;
    u32 BufferHandle23:16;
    
#else
    u32 Reserved0:8;
    u32 SIDHigh:8;
    u32 SIDLow:16;
    
    u32 CompletionFlags:16;
    u32 LoopID:16;
    
    u32 SequenceLength:16;
    u32 Class:16;
    
    u32 BufferHandle01:16;
    u32 BufferHandle00:16;
    
    u32 BufferHandle03:16;
    u32 BufferHandle02:16;
    
    u32 BufferHandle05:16;
    u32 BufferHandle04:16;
    
    u32 BufferHandle07:16;
    u32 BufferHandle06:16;
    
    u32 BufferHandle09:16;
    u32 BufferHandle08:16;
    
    u32 BufferHandle11:16;
    u32 BufferHandle10:16;
    
    u32 BufferHandle13:16;
    u32 BufferHandle12:16;
    
    u32 BufferHandle15:16;
    u32 BufferHandle14:16;
    
    u32 BufferHandle17:16;
    u32 BufferHandle16:16;
    
    u32 BufferHandle19:16;
    u32 BufferHandle18:16;
    
    u32 BufferHandle21:16;
    u32 BufferHandle20:16;
    
    u32 BufferHandle23:16;
    u32 BufferHandle22:16;
#endif

};

#define FARPTYPE32CWORDS  7
#define FARPTYPE32SENSE   FC_FALSE
#define FARPTYPE32CDB     FC_FALSE

typedef struct IPReceiveIOCBStruct tIPReceiveIOCB;

struct NameType
{
	u8 Name[8];
};

struct IPAddrType
{
	u8 Name[16];
};

struct PortIDType
{
	u8 ID[4];
};

struct TypeFARPIOCBStruct
{
    struct IOCBHeader   Header;
    u32 TransferHandle;      /* transfer handle */
#ifdef STRUCT_CASE1
    u32 Reserved1:16;
    u32 CompletionStatus:16;
    u32 Reserved2[3];
    u32 Reserved3:16;
	u32 MatchAddrCode:8;
	u32 RespActionCode:8;
#else
    u32 CompletionStatus:16;
    u32 Reserved1:16;
    u32 Reserved2[3];
	u32 RespActionCode:8;
	u32 MatchAddrCode:8;
    u32 Reserved3:16;
#endif
	struct PortIDType ReqPortID;
	WorldWideNameType PortName;
	WorldWideNameType NodeName;
	IPAddressType IPAddr;
};

typedef struct TypeFARPIOCBStruct tFARPIOCB;


#endif /* ifdef IP_SUPPORT */


#endif /* FCIOCB_H */
