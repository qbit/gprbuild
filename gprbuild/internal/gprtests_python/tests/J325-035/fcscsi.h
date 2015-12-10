/* ********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             SCSI.H    $Revision: 1.1 $
    Module Description:     SCSI definitions


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

#ifndef FCSCSI_H
#define FCSCSI_H


#define MAX_SCSI_ID     16

#define IMMED_RESOURCE_CNT     MaxConcurrentCommands
#define CMD_RESOURCE_CNT       MaxConcurrentCommands

/* sense and FCP status info */
#define SCSI_GOOD_STATUS    0
#define SCSI_CHK_COND       0x02
#define SCSI_RESV_STATUS    0x18
#define SCSI_BUSY_STATUS    0x08

#define REQ_SNS_LEN         0x12
#define INQUIRY_CMD_LEN     0x24

typedef struct  {
  u16  LUNDbFlags;
  u16  LUNDbRsvdID;
  u16  LUNDb3rdpID;
  u16  unused;
  }LUNDATABASE;
#define LUNDBFRSVD       0x0001
#define LUNDBFRSVD3      0x0002


typedef struct   {
  u8  diag_buf[0xc0];

  }DIAG_BUF;
#define DIAG_BUF_LEN 0xc0

/* global function prototypes */

void SendSCSICommand(u32 TransferHandle, TransferInfoType *TI, HostAdapterType *HostAdapter);

void Enable_LUN(u8 lun, HostAdapterType *HostAdapter);
void ProcessResponseQueue(HostAdapterType *HostAdapter);
FIBRE_TRANSFER_ERR Continue_Target_IO(TransferInfoType *TI, HostAdapterType *HostAdapter);

#endif /* FCSCSI_H */
