/* ********************************************************************************************

    COPYRIGHT 1998, 1999, 2000 DELPHI ENGINEERING GROUP,  INC.  ALL RIGHTS RESERVED

    File Name :             FCAMCD.H
    Module Description:     Defines for AMCD program interface


  Special Notes:

       
  ************************************************************************
  *                                                                      *
  *                              NOTICE                                  *
  *                                                                      *
  *       COPYRIGHT 1998,1999,2000 DELPHI ENGINEERING GROUP, INC         *
  *							2001, 2002 Broadband Storage, Inc.						 *
  *                          ALL RIGHTS RESERVED                         *
  *                                                                      *
  * This computer program is PROPRIETARY and contains	 *
  * TRADE SECRETS of Broadband Storage, INC.  The  receipt or possession *
  * of this program does not convey any rights to reproduce				 *
  * or disclose  its  contents,  or to manufacture, use, or sell         *
  * anything that it may describe, in whole or in part, without the		 *
  * specific written consent of Broadband Storage, INC. Any reproduction *
  * of this program without the express written consent of				 *
  * Broadband Storage, INC is a violation of the copyright laws and		 *
  * may subject you to civil liability and criminal prosecution.         *
  *                                                                      *
  ************************************************************************
******************************************************************************************** */

#ifndef FCAMCD_H
#define FCAMCD_H

#if 0
#ifdef __cplusplus
extern "C"{
#endif
#endif

#ifndef STRUCT_CASE1
#  ifndef BIG_ENDIAN
#    ifndef HIGH_2_LOW
#       define STRUCT_CASE1
#    endif
#  else
#    ifdef HIGH_2_LOW
#       define STRUCT_CASE1
#    endif
#  endif
#endif
/* lip error timeout values */
#define LIP_AL_TIME			0x0000
#define LISM_LP_TOV			0x1001
#define ARB_F0_LP_TOV		0x1002
#define LM_LIFA_LP_TOV		0x2003
#define LM_LIPA_LP_TOV		0x2005
#define LM_LIHA_LP_TOV		0x2007
#define LM_LISA_LP_TOV		0x2009
#define LM_LIRP_LP_TOV		0x200B
#define LM_LILP_LP_TOV		0x200D
#define LNM_LIPA_LP_TOV		0x2011
#define LNM_LIHA_LP_TOV		0x2013
#define LNM_LISA_LP_TOV		0x2015
#define LNM_LIRP_LP_TOV		0x2017
#define LNM_LILP_LP_TOV		0x2025
#define LNM_CLS_LP_TOV		0x2027

/* new status errors*/
#define E_D_TOV				0x0050
#define ACK_TOV				0x0051
#define BB_CREDIT_E_D_TOV	0x0052
#define SEQ_CNT_ERR			0x0053
#define RJT_RCVD			0x0054
#define BSY_RCVD			0x0055
#define ABTS_RCVD			0x0056
#define LOSS_SYNC			0x0057
#define AL_TIME 			0x0058
#define LP_TOV  			0x0059
#define LP_BB_CREDIT		0x005a
/* Testing field flags */
#define R_T_TOV_TEST_FLAG       (1<<0)      
#define E_D_TOV_TEST_FLAG		(1<<1)      
#define ACK_E_D_TOV_TEST_FLAG   (1<<2)
#define BB_CREDIT_TEST_FLAG		(1<<3)
#define SEQ_CNT_TEST_FLAG       (1<<4)
#define PRJT_TEST_FLAG          (1<<5)
#define BUSY_TEST_FLAG          (1<<6)
#define EOFa_TEST_FLAG          (1<<7)
#define ABTS_TEST_FLAG          (1<<8)
#define UNDERRRUN_TEST_FLAG     (1<<9)
#define OVERRRUN_TEST_FLAG      (1<<10)
#define START_DBG_TRC_FLAG      (1<<11)
#define STOP_DBG_TRC_FLAG       (1<<12)

/* firmware intial option  */
#define FAIRNESS_DISABLE_FLAG    	(1<<1)    
#define FAIRNESS_DISABLE_OPTION  	0xfffd    /* zero out only bit 1  */  
#define FORCE_LOGIN_FLAG    		(1<<2)    
#define FORCE_LOGIN_OPTION  		0x2000    /*  bit 13  */  
#define ENABLE_PORT_DB_AE_FLAG      (1<<8)
#define ENABLE_PORT_DB_AE_OPTION  	0x0100    /*  bit 8  */  
#define USE_GPIO_EN_FLAG			(1<<13)
#define FIFO_SNOOP_EN_FLAG			(1<<14)

/* BIT bits for GPIO FPGA interface */
#define GPIO_LD_ZERO_FLAG			(1<<0)
#define GPIO_LD_ZERO				0x0010
#define GPIO_LD_PAT1_FLAG			(1<<1)
#define GPIO_LD_PAT1				0x0014
#define GPIO_LD_PAT2_FLAG			(1<<2)
#define GPIO_LD_PAT2				0x0018
#define GPIO_LD_PAT3_FLAG			(1<<3)
#define GPIO_LD_PAT3				0x001C

/* additional mailbox task command codes */   
#define TgtReset			0x0066
#define AbtTskSet			0x0067
#define ClrTskSet			0x0068
#define LUNReset			0x007E
#define	AMCDAddOpts			0x0080

/* Clock Sync Server data struct */
#if 0
typedef struct _CLockDataType{
	u8 curCSU[8];       /* current CSU data */
	u8 oldCSU[8];       /* previous CSU data */
	u8 curCounter[4];   /* current CSHW counter (includes overflow flag) */
	u8 oldCounter[4];   /* prevoius CSHW counter (includes overflow flag) */
}CLockDataType;
#endif
typedef struct _CLockDataType{
	u32 CSUmsw;
	u32 CSUlsw;
	u32 RxCntr;
	u32 Rsvd;
	u32 RstCntr;
	u32 RtcCntr;
}CLockDataType;
#define CSU_DATA_SIZ sizeof(CLockDataType)

/*
SyHostEOFaErr: ds		1		; Local EOFa Error count
SyHostCRCErrCnt: ds		1		; Local CRC Error count
SyHostSyncErrCnt: ds	1		; Local Sync Error count
SyHostLossSigCnt: ds	1		; Local Loss of Signal Error count
SyHostABTSCnt:	ds		1		; Local ABTS rcvd count
*/
typedef struct _ErrorLogType{
	u16 eofa_rcvd_cnt;
	u16 bad_crc_cnt;
	u16 loss_sync_cnt;
	u16 loss_signal_cnt;
	u16 abts_err_cnt;
}ErrorLogType;
#define ERR_LOG_TYPE_SIZE sizeof(ErrorLogType)
/*
SyLinkFailCnt:       ds 2  	; Link Failure Count
SyLossOfSyncCnt:     ds	2	; Loss of sync count
SyLossOfSignalCnt:   ds 2  	; Loss of Signal Count
SyPrimSeqPrtclErr:   ds 2  	; Primitive Sequence Protocol Error
SyInvalidXmtWord:    ds 2  	; Invalid Transmission Word
SyCRCErr:            ds	2	; CRC error count
*/
typedef struct{
	u32 link_failure_cnt;
	u32 loss_sync_cnt;
	u32 loss_signal_cnt;
	u32 rsvd0;
	u32 rsvd1;
	u32 bad_crc_cnt;
}LESBType;
#define LESB_TYPE_SIZE sizeof(LESBType)


typedef struct{
#ifdef STRUCT_CASE1
	u8 rsvd0;
	u8 rsvd1;
	u16 rsvd2;
#else
	u16 rsvd2;
	u8 rsvd1;
	u8 rsvd0;
#endif
	u32 rsvd3;
	u32 rsvd4;
	u32 node_name_h;
	u32 node_name_l;
	u32 port_name_h;
	u32 port_name_l;
#ifdef STRUCT_CASE1
	u16 rsvd5;
	u16 rsvd6;
	u8 rsvd7;
	u8 rsvd8;
	u16 rsvd9;
	u16 rsvd10;
	u16 rsvd11;
	u16 rsvd12;
	u16 rsvd13;
	u16 rsvd14;
	u16 common_features;
	u16 total_conc_seq;
	u16 rsvd15;
	u16 recipient_ctl_flags;
	u16 rcv_data_size;
	u16 conc_seq;
	u16 open_seq_per_xchg;
	u16 rsvd16;
	u16 rsvd17;
	u16 rsvd18;
	u16 rsvd19;
	u16 rsvd20;
	u16 rsvd21;
	u16 rsvd22;
	u16 PRLI_payload_len;
	u16 wd0_bit15to0_PRLI_service_param; 
	u16 wd3_bit15to0_PRLI_service_param;
	u16 loop_id;
	u16 rsvd23;
	u16 rsvd24;
	u16 rsvd25;
	u16 rsvd26;
	u16 rsvd27;
#else
	u16 rsvd6;
	u16 rsvd5;
	u8 rsvd9;
	u8 rsvd8;
	u16 rsvd7;
	u16 rsvd11;
	u16 rsvd10;
	u16 rsvd13;
	u16 rsvd12;
	u16 common_features;
	u16 rsvd14;
	u16 rsvd15;
	u16 total_conc_seq;
	u16 rcv_data_size;
	u16 recipient_ctl_flags;
	u16 open_seq_per_xchg;
	u16 conc_seq;
	u16 rsvd17;
	u16 rsvd16;
	u16 rsvd19;
	u16 rsvd18;
	u16 rsvd21;
	u16 rsvd20;
	u16 PRLI_payload_len;
	u16 rsvd22;
	u16 wd3_bit15to0_PRLI_service_param; 
	u16 wd0_bit15to0_PRLI_service_param;
	u16 rsvd23;
	u16 loop_id;
	u16 rsvd25;
	u16 rsvd24;
	u16 rsvd27;
	u16 rsvd26;
#endif
	u32 FLOGI_RA_TOV;
	u32 FLOGI_E_D_TOV;
}LoginInfoType;

#define LOGIN_INFO_SIZE     sizeof(LoginInfoType)

/*  Function prototypes	*/

FIBRE_TRANSFER_ERR Fibre_Get_Login_Parameters(u8 HOST_ID, u16 PortId, u32 LoginParametersAddress, 
												u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID)); 
/*
FIBRE_TRANSFER_ERR Fibre_Get_Login_Parameters(u8 HOST_ID, u8 PortId, LoginInfoType *LoginParametersBuff, 
												u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID)); 
*/
FIBRE_TRANSFER_ERR Fibre_Get_Errors(u8 HOST_ID, ErrorLogType *ELOG, u32 CommandID, 
										void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Get_Exec_Cmnds(u8 HOST_ID, u32 CommandID, 
										void (*CallBackFunction)(u32 Status, u16 ExecCmnds, u16 NextData));

FIBRE_TRANSFER_ERR Fibre_Send_Debug_Cmd(u8 HOST_ID, u16 Command, u16 Addr, u16 Len, u32 CommandID, 
                                      void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Register_Debug_Addr(u8 HOST_ID, u32 DbgBuffPA, u32 CommandID, 
                                      void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Request_LESB(u8 HOST_ID, u16 PortId, u16 SwitchID, LESBType *LESB, u32 CommandID, 
										void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Set_Timeouts(u8 HOST_ID, u32 CommandID, u16 FC_AL_TIME, u16 FC_E_D_TOV, u16 FC_R_T_TOV, u16 FC_LP_TOV, 
										void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_INIT_ERR Fibre_Set_AdditFWOpts(u8 HOST_ID, u32 CommandID, u16 AddOpts, 
										void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Data_Rate(u8 HOST_ID, u32 CommandID, 
                                        void (*CallBackFunction)(u32 Status, u32 ID));
                                        
/*FIBRE_TRANSFER_ERR Fibre_Send_CSR(u8 HostID, u32 CommandID, 
                                        void (*CallBackFunction)(u32 Status, u32 ID));
*/
FIBRE_TRANSFER_ERR Fibre_Send_CSR(u8 HostID, u8 PortID, u32 CS_mode, u32 CS_update, 
                                  u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID));
                                        
FIBRE_TRANSFER_ERR Fibre_Get_RTC(u8 HostID, u32 RTCLogAddress, u32 CommandID, 
                                        void (*CallBackFunction)(u32 Status, u32 ID));
/*
FIBRE_TRANSFER_ERR Fibre_Get_RTC(u8 HostID, CLockDataType *RTCDataLog, u32 CommandID, 
                                        void (*CallBackFunction)(u32 Status, u32 ID));
*/
FIBRE_TRANSFER_ERR Fibre_Send_CSU(u8 HostID, u8 PortID, u8 *CSU_data, 
                                  u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Set_Test_Flags(u8 HOST_ID, u32 CommandID, u16 TestFlags, u16 TestFlags2, 
                                      void (*CallBackFunction)(u32 Status, u32 FlagsSet));

FIBRE_TRANSFER_ERR Fibre_Set_TMFlags(u8 HOST_ID,u8 PortId, u16 TMCmnd, u16 Subaddress, 
									 u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Set_RDMA_Params(u8 HOST_ID, u32 CommandID, u16 MaxOffset, 
                                      void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Force_LIP_Login(u8 HostID, u32 ID, void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Force_LIP_Reset(u8 HostID, u8 LoopId, u8 Delay, u32 ID, void (*CallBackFunction)(u32 Status, u32 ID));

FIBRE_TRANSFER_ERR Fibre_Get_XCB_4_IOCB(u8 HOST_ID, u32 CommandID, u16 Iocb, 
                                      void (*CallBackFunction)(u32 Status, u16 Xcb));

FIBRE_TRANSFER_ERR Fibre_Register_Trace_Debug_Addr(u8 HOST_ID, u32 DbgBuffPA, u32 BuffSize, u32 CommandID, 
													void (*CallBackFunction)(u32 Status, u32 Info));

FIBRE_TRANSFER_ERR Fibre_Set_CS_FPGA_Opts(u8 HOST_ID,  u16 Opts, u32 CommandID, 
										void (*CallBackFunction)(u32 Status, u32 ID));


#if 0
#ifdef __cplusplus
}
#endif
#endif

#endif