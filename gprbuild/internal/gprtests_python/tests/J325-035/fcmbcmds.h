/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FCMBCMDS.h   $Revision: 1.1 $
    Module Description:     FC-2100 Mailbox command defines

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



#ifndef FCMBCMDS_H
#define FCMBCMDS_H

#define MBC_NOP                 0x00
#define MBC_LOAD_RAM            0x01
#define MBC_EXE_FW              0x02
#define MBC_DUMP_RAM            0x03
#define MBC_WRT_RAM_WORD        0x04
#define MBC_RD_RAM_WORD         0x05
#define MBC_MB_REG_TEST         0x06
#define MBC_VER_CHKSUM          0x07
#define MBC_INIT_RISC_RAM_BOOT  0x08    /* boot firmware only */
#define MBC_GET_FW_VERSION      0x08
#define MBC_LOAD_RISC_RAM       0x09
#define MBC_DUMP_RISC_RAM       0x0A
#define MBC_LOAD_RISC_RAM_EXT_BOOT      0x0B  /*boot and 2300 and > only */
#define MBC_DUMP_RISC_RAM_EXT_BOOT      0x0C  /*boot and 2300 and > only */
#define MBC_CHKSUM_FW           0x0E
#define MBC_WRITE_RISC_WORD_EXT_BOOT    0x0D  /*boot and 2300 and > only */
#define MBC_INIT_RISC_RAM_EXT_BOOT      0x0E  /*boot and 2300 and > only */
#define MBC_READ_RISC_WORD_EXT_BOOT     0x0F  /*boot and 2300 and > only */
#define MBC_INIT_REQ_QUEUE      0x10
#define MBC_INIT_RESP_QUEUE     0x11
#define MBC_EXECUTE_CMD_IOCB    0x12
#define MBC_SET_WAKEUP          0x13
#define MBC_STOP_FW             0x14
#define MBC_ABORT_CMD_IOCB      0x15
#define MBC_ABORT_DEVICE        0x16
#define MBC_ABORT_TARGET        0x17
#define MBC_RESET               0x18
#define MBC_STOP_QUEUE          0x19
#define MBC_START_QUEUE         0x1A
#define MBC_SSTEP_QUEUE         0x1B
#define MBC_ABORT_QUEUE         0x1C
#define MBC_GET_QUEUE_STATUS    0x1D
#define MBC_GET_FW_STATUS       0x1F
#define MBC_GET_ID              0x20
#define MBC_GET_RETRY_COUNT     0x22
#define MBC_GET_TARGET_PARAMS   0x28
#define MBC_GET_PORT_QUEUE_PARAMS 0x29
#define MBC_SET_ID              0x30
#define MBC_SET_RETRY_COUNT     0x32
#define MBC_SET_CR              0x34
#define MBC_SET_TARGET_PARAMS   0x38
#define MBC_SET_PORT_QUEUE_PARAMS 0x39
#define MBC_INIT_FW             0x40
#define MBC_DIAGNOSTIC_ECHO     0x44
#define MBC_LOOPBACK_TEST       0x45
#define MBC_SET_BADR            0x48
#define MBC_SET_LLBA            0x49
#define MBC_LOAD_RISC_RAM_ALT   0x50
#define MBC_DUMP_RISC_RAM_ALT   0x51
#define MBC_RESET_LINK_STATUS   0x52
#define MBC_EXECUTE_CMD_IOCB64  0x54
#define MBC_ENB_TARGET          0x55
#define MBC_OMINPORT_CONFIG     0x5A
#define MBC_SET_RDMA_BASE       0x5B
#define MBC_FIRMWARE_HB			0x5C
#define MBC_DATA_RATE           0x5D
                
#define MBC_GET_INIT_CNTRL_BLK  0x61
#define MBC_INITIATE_LIP        0x62
#define MBC_GET_POS_MAP         0x63
#define MBC_GET_PORT_DB         0x64
#define MBC_CLEAR_ACA           0x65
#define MBC_TARGET_RESET        0x66
#define MBC_CLEAR_TASK_SET      0x67
#define MBC_ABORT_TASK_SET      0x68
#define MBC_GET_FW_STATE        0x69
#define MBC_GET_PORT_NAME       0x6A
#define MBC_GET_LINK_STATUS     0x6B
#define MBC_SEND_SNS            0x6E
#define MBC_LOGIN_FABRIC_PORT   0x6F
#define MBC_INITIATE_LIP_RESET  0x6C
#define MBC_LOGOUT_FABRIC_PORT  0x71
#define MBC_INITIATE_LIP_LOGIN  0x72
#define MBC_LOGIN_LOOP_PORT     0x74
#define MBC_GET_NAME_ID_LIST    0x75
#define MBC_INIT_IP             0x77
#define MBC_LUN_RESET           0x7E

#define MBC_HOST_SET_TOVS		0x7F
#define MBC_GET_ERRORS			0x80
#define MBC_REQUEST_CSR         0x81
#define MBC_REQUEST_RTC         0x82
#define	MBC_SET_CS_FPGA_OPTS	0x83
#define MBC_SEND_CSU            0x84
#define	MBC_SET_TEST_FLAGS		0x85
#define MBC_SET_RDMA_SUBADDR	0x86
#define MBC_SET_RDMA_MULTIPLE	0x87
#define	MBC_GET_SUBADDR_LIST	0x88
#define	MBC_GET_SUBADDR_SINGLE	0x89
#define MBC_SEND_SW_KEY			0x8a
#define	MBC_SEND_DEBUG_CMD		0x8C
#define MBC_REG_DBG_ADDR		0x8D
#define MBC_GET_TRC_ADDR_N_SIZE	0x8E
#define MBC_REG_TRC_ADDR		0x8F
#define MBC_DRIVER_HB			0x90
#define MBC_GET_SENSOR_TEMP		0x91
#define MBC_GET_SENSOR_A_D		0x92
#define MBC_IMPLICIT_PORT_DEF   0x01E0
#define MBC_REGISTER_FAST_IP_BUFFER 0x01E1
#define MBC_ENABLE_WATCHDOG     0x01E2

/* Mailbox command status returned from 2100 */

#define MB_STATUS_GOOD              0x4000
#define MB_STATUS_INVALID           0x4001
#define MB_STATUS_INTERFACE_ERROR   0x4002
#define MB_STATUS_TEST_FAILED       0x4003
#define MB_STATUS_COMMAND_ERROR     0x4005
#define MB_STATUS_PARAM_ERROR       0x4006
#define MB_STATUS_INVALID_KEY       0x400A
#define MB_STATUS_LOOPUP_REQUIRED   0x400B  /* 400B and 400C are loopback test errors */
#define MB_STATUS_LOOPBACK_ERR      0x400C
#define MB_STATUS_SEQ_CHKSUM_ERR    0x4010

/* Asynchronous Event Notification Codes */

#define AE_RESET_DETECTED           0x8001
#define AE_SYSTEM_ERROR             0x8002
#define AE_REQ_Q_TRANSFER_ERROR     0x8003
#define AE_RESP_Q_TRANSFER_ERROR    0x8004
#define AE_REQ_Q_WAKEUP             0x8005
#define AE_LIP_OCCURRED             0x8010
#define AE_LOOP_UP                  0x8011
#define AE_LOOP_DOWN                0x8012
#define AE_LIP_RESET                0x8013
#define AE_DATABASE_CHANGED         0x8014
#define AE_CHANGE_NOTIFICATION      0x8015
#define AE_LIP_ERROR		        0x8017
#define AE_COMMAND_COMPLETE         0x8020
#define AE_CTIO_COMPLETE            0x8021
#define AE_IP_TRANSMIT_COMPLETE     0x8022
#define AE_IP_RECEIVE_COMPLETE      0x8023
#define AE_IP_BROADCAST_RECEIVE     0x8024
#define AE_NO_IP_RCV_BUFFERS        0x8026
#define AE_IP_RCV_BUF_Q_EMPTY       0x8027
#define AE_CONNECTED_P2P            0x8030
#define AE_RCV_Q_FULL       		0x8049
#define AE_RDMA_RECEIVE_COMPLETE    0x8080
#define AE_RDMA_SEND_COMPLETE       0x8081
#define AE_WATCHDOG_TIMEOUT         0x8082
#define AE_R_T_TOV					0x8083
#define AE_CSU_RCVD                 0x8084
#define AE_CSR_RCVD                 0x8085 /* test only CSR NIC to NIC*/
#define AE_AL_TIME					0x8086
#define AE_LP_TOV					0x8087
#define AE_BOUNDARY_VIOL			0x8088
#define AE_PLOGI_RCVD				0x8089
#define AE_PRLI_RCVD				0x8090
#define AE_PRLI_ACC_RCVD			0x8091
#define AE_SEQ_INT					0x8092
#define AE_SEQ_RET					0x8093
#define AE_ACK_SENT					0x8094
#define AE_XFER_VIOL				0x8095
#define AE_PDISC_RCVD				0x8096
#define AE_PDISC_ACC_RCVD			0x8097
#define AE_READY					0x8098
#define BP_ACTIVE					0x8099
#define AE_LOS						0x809a
#define AE_DMP_TRC_RCVD				0x809b
#define AE_DMP_TRC_START			0x809c
#define AE_DMP_TRC_STOP				0x809d
#define AE_DMP_BUFF_SND_FAIL		0x809e

#define INIT_RAM_LENGTH 4096                 /*number of words that can be set in Init Risc Ram MBC */


#define IFWCBCWORDS  24
#define IFWCBSENSE   FC_FALSE
#define IFWCBCDB     FC_FALSE

#define DEL_OPT_IMPLICIT_LIP (1<<7)
#define DEL_OPT_IMPLICIT_LOGIN (1<<6)

struct IFWCBStruct
{
#ifdef STRUCT_CASE1
    u32     Version:8;
    u32     DelphiOptions:8;
    u32     Options:16;
    u32     FrmLength:16;
    u32     MaxResourceAlloc:16;
    u32     Throttle:16;
    u32     RetryCount:8;
    u32     RetryDelay:8;
    u32     PortName0:8;
    u32     PortName1:8;
    u32     PortName2:8;
    u32     PortName3:8;
    u32     PortName4:8;
    u32     PortName5:8;
    u32     PortName6:8;
    u32     PortName7:8;
    u32     HardAddress:16;
    u32     Rsvd0:16;
    u32     NodeName0:8;
    u32     NodeName1:8;
    u32     NodeName2:8;
    u32     NodeName3:8;
    u32     NodeName4:8;
    u32     NodeName5:8;
    u32     NodeName6:8;
    u32     NodeName7:8;
    u32     ReqQOutIndex:16;
    u32     RspQInIndex:16;
    u32     ReqQLength:16;
    u32     RspQLength:16;
    u32     ReqQAddress0;
    u32     ReqQAddress1;
    u32     RspQAddress0;
    u32     RspQAddress1;
    /* extended firmware options */
    u32     LunEnables:16;
    u32     CommandRsrcCount:8;
    u32     ImmedNotifyCount:8;

    u32     Timeout:16;
    u32     ReservedA:16;

    u32     AdditFWOptions:16;
    u32     RespAccTimer:8;
    u32     IntDelayTimer:8;

    u32     SpecialOptions:16;
    u32     Rsvd5:16;

    u32     Rsvd6;
    u32     Rsvd7;
    u32     Rsvd8;
    u32     Rsvd9;
    u32     Rsvd10;
    u32     Rsvd11;
#else
    u32     Options:16;
    u32     DelphiOptions:8;
    u32     Version:8;
    u32     MaxResourceAlloc:16;
    u32     FrmLength:16;
    u32     RetryDelay:8;
    u32     RetryCount:8;
    u32     Throttle:16;
    u32     PortName3:8;
    u32     PortName2:8;
    u32     PortName1:8;
    u32     PortName0:8;
    u32     PortName7:8;
    u32     PortName6:8;
    u32     PortName5:8;
    u32     PortName4:8;
    u32     Rsvd0:16;
    u32     HardAddress:16;
    u32     NodeName3:8;
    u32     NodeName2:8;
    u32     NodeName1:8;
    u32     NodeName0:8;
    u32     NodeName7:8;
    u32     NodeName6:8;
    u32     NodeName5:8;
    u32     NodeName4:8;
    u32     RspQInIndex:16;
    u32     ReqQOutIndex:16;
    u32     RspQLength:16;
    u32     ReqQLength:16;
    u32     ReqQAddress0;
    u32     ReqQAddress1;
    u32     RspQAddress0;
    u32     RspQAddress1;

    /* extended firmware options */
    u32     ImmedNotifyCount:8;
    u32     CommandRsrcCount:8;
    u32     LunEnables:16;

    u32     ReservedA:16;
    u32     Timeout:16;

    u32     IntDelayTimer:8;
    u32     RespAccTimer:8;
    u32     AdditFWOptions:16;

    u32     Rsvd5:16;
    u32     SpecialOptions:16;

    u32     Rsvd6;
    u32     Rsvd7;
    u32     Rsvd8;
    u32     Rsvd9;
    u32     Rsvd10;
    u32     Rsvd11;

#endif
};

typedef struct IFWCBStruct tIFWControlBlock;


#ifdef IP_SUPPORT
/* Initialize IP Control Block */
#define IIPCBWORDS  16
#define IIPCBSENSE   FC_FALSE
#define IIPCBCDB     FC_FALSE

struct IIPCBStruct
{
#ifdef STRUCT_CASE1
    u32 Version:8;
    u32 Reserved:8;
    u32 Options:16;

    u32 IPHeaderSize:16;
    u32 MaxTUSize:16;

    u32 BufferSize:16;
    u32 Reserved1:16;

    u32 Reserved2:16;
    u32 Reserved3:16;

    u32 Reserved4:16;
    u32 RcvBufQSize:16;

    u32 LowWaterMark:16;
    u32 RcvBufQAddress0:16;
    
    u32 RcvBufQAddress1:16;
    u32 RcvBufQAddress2:16;

    u32 RcvBufQAddress3:16;
    u32 RcvBufQInPtr:16;
    
    u32 FastPostCount:16;
    u32 FWRcvBufContainerCount:16;

#else
    u32     Options:16;
    u32     Reserved:8;
    u32     Version:8;

    u32 MaxTUSize:16;
    u32 IPHeaderSize:16;

    u32 Reserved1:16;
    u32 BufferSize:16;

    u32 Reserved3:16;
    u32 Reserved2:16;

    u32 RcvBufQSize:16;
    u32 Reserved4:16;

    u32 RcvBufQAddress0:16;
    u32 LowWaterMark:16;
    
    u32 RcvBufQAddress2:16;
    u32 RcvBufQAddress1:16;

    u32 RcvBufQInPtr:16;
    u32 RcvBufQAddress3:16;
    
    u32 FWRcvBufContainerCount:16;
    u32 FastPostCount:16;

#endif

    u32 ReservedX[7];        
};

typedef struct IIPCBStruct tIIPControlBlock;
#endif /* ifdef IP_SUPPORT */

struct PortDatabaseStruct
{
    u8      Options;
    u8      Control;
    u8      MasterState;
    u8      SlaveState;
    u32     HardAddress;
    u32     PortID;
    u8      NodeName[8];
    u8      PortName[8];
    u16     ExecutionThrottle;
    u16     ExecutionCount;
    u8      RetryCount;
    u8      Reserved1;
    u16     ResourceAllocation;
    u16     CurrentAllocation;
    u16     QueueHead;
    u16     QueueTail;
    u16     TransmitExecListNext;
    u16     TransmitExecListPrev;
    u16     CommonFeatures;
    u16     TotalConcurrentSequences;
    u16     RObyInfoCatFlags;
    u16     IRControlFlags;
    u16     ReceiveDataSize;
    u16     NumConcurrentSequences;
    u16     NumOpenSequencesPerExchange;
    u16     LUNAbortFlags;
    u16     LUNStopFlags;
    u16     StopQueueHead;
    u16     StopQueueTail;
    u16     PortRetryTimer;
    u16     NextSequenceID;
    u16     FrameCount;
    u16     PRLIPayloadLength;
    u16     PRLIServiceParamWord1;
};

typedef struct PortDatabaseStruct tPortDatabase;

#define IMPLICITDEFWORDS    3
#define IMPLICITDEFSENSE    FC_FALSE
#define IMPLICITDEFCDB      FC_FALSE

struct ImplicitDefStruct
{
#ifdef STRUCT_CASE1
    u32 PortID;
    u32 RxDataSize:16; /* Max Rx Frame Size */
    u32 Control:16;

#else
    u32 PortID;
    u32 Control:16;
    u32 RxDataSize:16;
#endif


};

typedef struct ImplicitDefStruct ImplicitPortDefType;


#endif /* FCMBCMDS_H  */
