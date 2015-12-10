/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FC2100.H    $Revision: 1.2 $
    Module Description:     Defines for FC-2100 hardware interface


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

#ifndef FC2100_H
#define FC2100_H


/************************************************************************/
/*      Bus Interface Registers                                         */
/************************************************************************/
#if (defined ISP2300 || defined ISP2322)
#define IspFlshAdr      0x0000
#define IspFlshData     0x0002
#define IspCtlSts       0x0006
#define Isp2PciIntCtl   0x0008

#define Isp2PciIntSts   0x000a

#define IspSemaphore    0x000c

#define IspNVRAM        0x000e
#define IspAcc          0x0080      /* accumulator      */
#define IspFPMVer       0x0080      /* requires FPM module select in IspCtlSts = 2 */
                                    /* hardware revision level <= 2 is 2100, 3 is 2100A, 4 is isp2200, 5 is 2200A, >=6 is 2300 */
#define IspFPMDiagCfg   0x0096
#define ISP_FPM_RESET   0x100       /* bit 8 set to one resets the FPM */
     
#define IspPCR          0x00A4
#define IspPSR          0x00A0
#define IspPC           0x00AC      /* program counter  */
#define IspGPIO         0x00CC
#define IspGPIOEnable   0x00CE

#define IspHccr         0x00C0
#define IspRiscBrkPnt0  0x00C2
#define IspRiscBrkPnt1  0x00C4

#define IspReqInPtr     0x0010      /* used to be MB4 */
#define IspReqQOutPtr   0x0012
#define IspRespQInPtr   0x0014
#define IspRespQOutPtr  0x0016      /* used to be MB5 */
#define IspStatus       0x0018
#define IspStatusLow    0x0018
#define IspStatusHigh   0x001A


/* 23xx MBCs */
#define Mailbox0    0x40 
#define Mailbox1    0x42  
#define Mailbox2    0x44  
#define Mailbox3    0x46  
#define Mailbox4    0x48  
#define Mailbox5    0x4A
#define Mailbox6    0x4C  
#define Mailbox7    0x4E
#define Mailbox8    0x50        
#define Mailbox9    0x52
#define Mailbox10   0x54
#define Mailbox11   0x56
#define Mailbox12   0x58
#define Mailbox13   0x5A
#define Mailbox14   0x5C
#define Mailbox15   0x5E
#define Mailbox16   0x60
#define Mailbox17   0x62
#define Mailbox18   0x64
#define Mailbox19   0x66
#define Mailbox20   0x68
#define Mailbox21   0x6A
#define Mailbox22   0x6C
#define Mailbox23   0x6E
#define Mailbox24   0x70
#define Mailbox25   0x72
#define Mailbox26   0x74
#define Mailbox27   0x76
#define Mailbox28   0x78
#define Mailbox29   0x7A
#define Mailbox30   0x7C
#define Mailbox31   0x7E
#endif





/************* 24XX defines *****************/

#ifdef ISP24XX
#define IspFlshAdr      0x0000      /* these are now 32 bit registers */
#define IspFlshData     0x0004
#define IspCtlSts       0x0008
#define Isp2PciIntCtl   0x000C

#define Isp2PciIntSts   0x0010

#define IspReqInPtr     0x001C      /* used to be MB4 */
#define IspReqQOutPtr   0x0020
#define IspRespQInPtr   0x0024
#define IspRespQOutPtr  0x0028      /* used to be MB5 */
#define IspPriorityReqQInPtr  0x002C
#define IspPriorityReqQOutPtr 0x0030
#define IspStatus       0x0044
#define IspStatusLow    0x0044
#define IspStatusHigh   0x0046
#define IspHccr         0x0048

#define Mailbox0    0x80 
#define Mailbox1    0x82  
#define Mailbox2    0x84  
#define Mailbox3    0x86  
#define Mailbox4    0x88  
#define Mailbox5    0x8A
#define Mailbox6    0x8C  
#define Mailbox7    0x8E
#define Mailbox8    0x90        
#define Mailbox9    0x92
#define Mailbox10   0x94
#define Mailbox11   0x96
#define Mailbox12   0x98
#define Mailbox13   0x9A
#define Mailbox14   0x9C
#define Mailbox15   0x9E
#define Mailbox16   0xA0
#define Mailbox17   0xA2
#define Mailbox18   0xA4
#define Mailbox19   0xA6
#define Mailbox20   0xA8
#define Mailbox21   0xAA
#define Mailbox22   0xAC
#define Mailbox23   0xAE
#define Mailbox24   0xB0
#define Mailbox25   0xB2
#define Mailbox26   0xB4
#define Mailbox27   0xB6
#define Mailbox28   0xB8
#define Mailbox29   0xBA
#define Mailbox30   0xBC
#define Mailbox31   0xBE

#define ISP24XX_RISC_INT_PENDING  0x8000 /* Isp2PciIntSts register */
#define ISP23xx_RISC_INT_PENDING  0x8000 /* Since it is the same, easier than moding rest of API */


#define RESET_RISC      0x10000000
#define PAUSE_RISC      0x30000000
#define RELEASE_RISC    0x40000000

#define HCTLNOP         0x00000000      /* No Operation                     */
#define HCTLRESETRISC   0x10000000      /* Reset the RISC Processor         */
#define HCTLCLRRISCRESET 0x20000000     /* Clear Risc Reset                 */
#define HCTLPAUSERISC   0x30000000      /* Pause the RISC Processor         */
#define HCTLRLSRISC     0x40000000      /* Release RISC from Reset or Pause */
#define HCTLSETH2RINTR  0x50000000      /* Set the Host to RISC Interrupt   */
#define HCTLCLRH2RINTR  0x60000000      /* Clear the Host to RISC Interrupt */
#define HCTLCLRR2HINTR  0xA0000000      /* Clear the RISC to Host Interrupt */
    
#endif


/************************************************************************/
/*                                                                      */
/*  ISP Control Status Register Definitions (IspCtlSts)                 */
/*                                                                      */
/************************************************************************/

#define ISP_SOFT_RESET      0x0001
#define MODULE_SELECT_MASK  0x0030
#define RISC_MODULE_SELECT  0x0000
#define FB_MODULE_SELECT    0x0010
#define FPM0_MODULE_SELECT  0x0020
#define FPM1_MODULE_SELECT  0x0030

/************************************************************************/
/*                                                                      */
/*  ISP to PCI Interrupt Control Register Definitions (Isp2PciIntCtl)   */
/*                                                                      */
/************************************************************************/

#define ENABLE_ALL_INTS     0x8000
#define ENABLE_FPM_INT      0x0020
#define ENABLE_FB_INT       0x0010
#define ENABLE_RISC_INT     0x0008
#define ENABLE_CDMA_INT     0x0004
#define ENABLE_RDMA_INT     0x0002
#define ENABLE_TDMA_INT     0x0001

/************************************************************************/
/*                                                                      */
/*  ISP to PCI Interrupt Status Register Definitions (Isp2PciIntSts)    */
/*                                                                      */
/************************************************************************/

#define ISP2100_INT_PENDING 0x8000
#define FPM_INT_PENDING     0x0020
#define FB_INT_PENDING      0x0010
#define RISC_INT_PENDING    0x0008
#define CDMA_INT_PENDING    0x0004
#define RDMA_INT_PENDING    0x0002
#define TDMA_INT_PENDING    0x0001

/************************************************************************/
/*                                                                      */
/*  ISP Status Register Definitions (IspStatus)                         */
/*                                                                      */
/************************************************************************/

#define ISP23xx_RISC_INT_PENDING    0x8000

/************************************************************************/
/*                                                                      */
/*  Semaphore Register Definitions (IspSemaphore)                       */
/*                                                                      */
/************************************************************************/

#define SEMAPHORE_UNLOCK    0x0000      /* unlock semaphore reg.        */
#define SEMAPHORE_LOCK      0x0001      /* lock semaphore reg.          */
#define SEMAPHORE_STATUS    0x0002      /* reflects previous stat of lock bit */


/************************************************************************/
/*                                                                      */
/* HCCR - Host Command & Control Register definitions                   */
/*                                                                      */
/************************************************************************/

/************************************************************************/
/*      Host Command/Control Register Bit Definitions                   */
/************************************************************************/

#define HCTLBRKPT0  0x0004      /* Bit for Enable/Disable Breakpoint 0  */
#define HCTLBRKPT1  0x0008      /* Bit for Enable/Disable Breakpoint 1  */
#define HCTLBRKPTEXT 0x0010     /* Bit for Enable/Disable Ext Brkpoint  */
#define HCTLRPAUSED 0x0020      /* RISC is in Pause Mode                */
#define HCTLRRESET  0x0040      /* RISC is in Reset Mode                */
#define HCTLH2RINTR 0x0080      /* Host to RISC Interrupt Status        */

/************************************************************************/
/*      Host Command/Control Register Command Definitions               */
/************************************************************************/
#ifndef ISP24XX
#define RESET_RISC  0x1000
#define PAUSE_RISC  0x2000
#define RELEASE_RISC 0x3000

#define HCTLNOP         0x0000      /* No Operation                     */
#define HCTLRESETRISC   0x1000      /* Reset the RISC Processor         */
#define HCTLPAUSERISC   0x2000      /* Pause the RISC Processor         */
#define HCTLRLSRISC     0x3000      /* Release RISC from Reset or Pause */
#define HCTLSTEPRISC    0x4000      /* Single Step the RISC             */
#define HCTLSETH2RINTR  0x5000      /* Set the Host to RISC Interrupt   */
#define HCTLCLRH2RINTR  0x6000      /* Clear the Host to RISC Interrupt */
#define HCTLCLRR2HINTR  0x7000      /* Clear the RISC to Host Interrupt */
#define HCTLDISABLEBIOS 0x9000      /* Disable BIOS access              */
#define HCTLWRPBFIELD   0xa000      /* Write to Breakpoint Field        */
#endif

#ifndef ISP24XX
#define SET_HOST2RISC_INTR(HostAdapterAddress)      FCWrite16(HostAdapterAddress + IspHccr, HCTLSETH2RINTR)
#define CLEAR_RISC2HOST_INTR(HostAdapterAddress)    FCWrite16(HostAdapterAddress + IspHccr, HCTLCLRR2HINTR)
#define RESET_RISCfn(HostAdapterAddress)              FCWrite16(HostAdapterAddress + IspHccr, HCTLRESETRISC)
#define RELEASE_RISCfn(HostAdapterAddress)            FCWrite16(HostAdapterAddress + IspHccr, HCTLRLSRISC)
#define PAUSE_RISCfn(HostAdapterAddress)              FCWrite16(HostAdapterAddress + IspHccr, HCTLPAUSERISC)
#else
#define SET_HOST2RISC_INTR(HostAdapterAddress)      FCWrite32(HostAdapterAddress + IspHccr, HCTLSETH2RINTR)
#define CLEAR_RISC2HOST_INTR(HostAdapterAddress)    FCWrite32(HostAdapterAddress + IspHccr, HCTLCLRR2HINTR)
#define RESET_RISCfn(HostAdapterAddress)              FCWrite32(HostAdapterAddress + IspHccr, HCTLRESETRISC)
#define RELEASE_RISCfn(HostAdapterAddress)            FCWrite32(HostAdapterAddress + IspHccr, HCTLRLSRISC)
#define PAUSE_RISCfn(HostAdapterAddress)              FCWrite32(HostAdapterAddress + IspHccr, HCTLPAUSERISC)
#endif


/* function prototypes */

WorldWideNameType GetWorldWideName(HostAdapterType *HostAdapter);
Boolean LoadRiscFirmware(u16 *FirmwareBuffer, u16 FirmwareLength, HostAdapterType *HostAdapter);
Boolean LoadRiscSeqFirmware(u16 *SeqFwBuffer, u16 Length, u32 SeqStartAddress, HostAdapterType *HostAdapter);
u16 DMARiscFirmware(u32 Firmware_Buffer_PA, u16 FirmwareLength, HostAdapterType *HostAdapter);
u16 ReadRiscFirmware(u32 Firmware_Buffer_PA, u16 FirmwareLength, HostAdapterType *HostAdapter);
u16 ExecuteRiscFirmware(HostAdapterType *HostAdapter);
u16 InitializeFirmware(HostAdapterType *HostAdapter);
Boolean VerifyRiscFwChecksum(HostAdapterType *HostAdapter);
s16 WaitForMboxReady(BaseAddressType HostAdapterAddress);
u16 WaitForMboxCmdCmpltn(HostAdapterType *HostAdapter);
u16 WaitForMbCmdCmpltServiceAEs(HostAdapterType *HostAdapter);
Boolean ServiceRiscInterrupt(HostAdapterType *HostAdapter);
void GetFwState(HostAdapterType *HostAdapter);
void ForceLIP(BaseAddressType HostAdapterAddress);
s16 GetLoopID(BaseAddressType HostAdapterAddress);
Boolean GetPortName(s16 PortID, BaseAddressType HostAdapterAddress);
Boolean GetPositionMap(u32 BufferHigh, u32 BufferLow, BaseAddressType HostAdapterAddress);
Boolean GetPortDatabase(s16 PortID, u32 BufferHigh, u32 BufferLow, BaseAddressType HostAdapterAddress);
void RISC_Access (u16 RISCAddress, s16 TransferDirection, u16 *Data, HostAdapterType *HostAdapter);
void RISC_Ext_Access (u32 RISCAddress, s16 TransferDirection, u16 *Data, HostAdapterType *HostAdapter);
void InitRiscRam(u16 StartAddress, u16 Length, u16 Value, HostAdapterType *HostAdapter);
void LoadRiscRamExtended(u32 RISCAddress, u32 SourceAddressPA, u16 Length, HostAdapterType *HostAdapter);
void DumpRiscRamExtended(u32 RISCAddress, u32 DestAddressPA, u16 Length, HostAdapterType *HostAdapter);
u16 ReadRiscRamExtended(u32 RISCAddress, HostAdapterType *HostAdapter);
void WriteRiscRamExtended(u32 RISCAddress, u16 Data, HostAdapterType *HostAdapter);

Boolean ProcessPortMapResponse(HostAdapterType *HostAdpater);
void SendMarker(HostAdapterType *HostAdapter, u16 Modifier);
void Incr_RespQ_Outindex(HostAdapterType *HostAdapter);
void Incr_ReqQ_Inindex(HostAdapterType *HostAdapter);
void Incr_ReqQ_Inindex_with_Cont(HostAdapterType *HostAdapter, u16 NumEntries);

#ifdef BIG_ENDIAN
u16 byte_swap16(u16 value);
u32 word_swap32(u32 value);
void BigEndConvert (u32 base[], u32 LWords2Convert, Boolean HasSenseField, Boolean HasCDBArray, Boolean HasHeader);
void BigEndConvert16 (u16 base[], u32 Words2Convert);
#endif

#if defined CHECK_SYNCHRONIZATION
void CheckSynchronization(HostAdapterType *HostAdapter);
void ClearCheckSynchronization(HostAdapterType *HostAdapter);
#endif

MBCQueueErrorType MBCQueueInsertItem(HostAdapterType *HostAdapter, MBCQueueItemType *MBCQueueItem);
MBCQueueErrorType MBCQueueExecuteHeadItem(HostAdapterType *HostAdapter);

KeyType Fibre_Read_Key(HostAdapterType *HostAdapter);
Boolean Send_Soft_Key(HostAdapterType *HostAdapter, KeyType Key);

#endif /* FC2100_H */
