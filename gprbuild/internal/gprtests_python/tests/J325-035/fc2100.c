/* ********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FC2100.C    $Revision: 1.7 $
    Module Description:     Provides routines to interface to the FC-2100 hardware and firmware

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
#include "eeprom.h"


/* maps AL_PA to Loop ID - invalid returns 0x7F */
static const u8 AL_PA_TO_ID[] =
{
0X7E, 0X7D, 0X7C, 0X7F, 0X7B, 0X7F, 0X7F, 0X7F, 0X7A, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X79,
0X78, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X77, 0X76, 0X7F, 0X7F, 0X75, 0X7F, 0X74, 0X73, 0X72,
0X7F, 0X7F, 0X7F, 0X71, 0X7F, 0X70, 0X6F, 0X6E, 0X7F, 0X6D, 0X6C, 0X6B, 0X6A, 0X69, 0X68, 0X7F,
0X7F, 0X67, 0X66, 0X65, 0X64, 0X63, 0X62, 0X7F, 0X7F, 0X61, 0X60, 0X7F, 0X5F, 0X7F, 0X7F, 0X7F,
0X7F, 0X7F, 0X7F, 0X5E, 0X7F, 0X5D, 0X5C, 0X5B, 0X7F, 0X5A, 0X59, 0X58, 0X57, 0X56, 0X55, 0X7F,
0X7F, 0X54, 0X53, 0X52, 0X51, 0X50, 0X4F, 0X7F, 0X7F, 0X4E, 0X4D, 0X7F, 0X4C, 0X7F, 0X7F, 0X7F,
0X7F, 0X7F, 0X7F, 0X4B, 0X7F, 0X4A, 0X49, 0X48, 0X7F, 0X47, 0X46, 0X45, 0X44, 0X43, 0X42, 0X7F,
0X7F, 0X41, 0X40, 0X3F, 0X3E, 0X3D, 0X3C, 0X7F, 0X7F, 0X3B, 0X3A, 0X7F, 0X39, 0X7F, 0X7F, 0X7F,
0X38, 0X37, 0X36, 0X7F, 0X35, 0X7F, 0X7F, 0X7F, 0X34, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X33,
0X32, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X31, 0X30, 0X7F, 0X7F, 0X2F, 0X7F, 0X2E, 0X2D, 0X2C,
0X7F, 0X7F, 0X7F, 0X2B, 0X7F, 0X2A, 0X29, 0X28, 0X7F, 0X27, 0X26, 0X25, 0X24, 0X23, 0X22, 0X7F,
0X7F, 0X21, 0X20, 0X1F, 0X1E, 0X1D, 0X1C, 0X7F, 0X7F, 0X1B, 0X1A, 0X7F, 0X19, 0X7F, 0X7F, 0X7F,
0X7F, 0X7F, 0X7F, 0X18, 0X7F, 0X17, 0X16, 0X15, 0X7F, 0X14, 0X13, 0X12, 0X11, 0X10, 0X0F, 0X7F,
0X7F, 0X0E, 0X0D, 0X0C, 0X0B, 0X0A, 0X09, 0X7F, 0X7F, 0X08, 0X07, 0X7F, 0X06, 0X7F, 0X7F, 0X7F,
0X05, 0X04, 0X03, 0X7F, 0X02, 0X7F, 0X7F, 0X7F, 0X01, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X00,
0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F, 0X7F
};

/* local function prototype */
void HandleAsyncEvent(HostAdapterType *HostAdapter, u16 Event);


/* *******************************************************************************************
    GetWorldWideName

    Description:
        Extracts the adapters world wide name from the eeprom on the card

    Parameters:
        none

    Return Value:
        WorldWideNameType:       The structure contains the WWN

    Notes:

******************************************************************************************* */
WorldWideNameType GetWorldWideName(HostAdapterType *HostAdapter)
{
    WorldWideNameType WWN;
    u16 Temp;
    u8 i;

    if ((FCRead16(HostAdapter->BaseAddress + IspCtlSts) & 0xC000) == 0x4000) /* check if port 2 of 2312 */
    for (i = 0; i < sizeof(WWN)/2; i++)
    {
        Temp = ReadEEPROM(i+EEPROM_WWN_START+0x80,HostAdapter->BaseAddress); /* wwn is in upper bank of eeprom */
        WWN.Bytes[i*2] = Temp & 0xFF;
        WWN.Bytes[(i*2) + 1] = (Temp >> 8) & 0xFF;
    }
    else
    for (i = 0; i < sizeof(WWN)/2; i++)
    {
        Temp = ReadEEPROM(i+EEPROM_WWN_START,HostAdapter->BaseAddress);
        WWN.Bytes[i*2] = Temp & 0xFF;
        WWN.Bytes[(i*2) + 1] = (Temp >> 8) & 0xFF;
    }

    return WWN;
}

/* *******************************************************************************************
    LoadRiscFirmware

    Description:
        Loads the RISC firmware into the RAM on the FC-2100 card

    Parameters:
        u16 *FirmwareBuffer:    Pointer to the 16 bit wide firmware buffer
        u16 FirmwareLength:     The length of the firmware buffer
        BaseAddressType HostAdapterAddress: Base address of HostAdapter

    Return Value:
        None

    Notes:

******************************************************************************************* */

Boolean LoadRiscFirmware(u16 *FirmwareBuffer, u16 FirmwareLength, HostAdapterType *HostAdapter)
{
    u32 Address = FIRMWARE_START_ADDRESS;
	BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    FCDEBUGPRINT(("\nLoading firmware via PIO\n"));
	    while (FirmwareLength > 0)
	    {
/*          RISC_Access(Address++,RISC_WRITE,FirmwareBuffer++, HostAdapterAddress);*/     /* write data into FC-2100 RISC RAM     */
	        WaitForMboxReady(HostAdapterAddress);
	        FCWrite16(HostAdapterAddress + Mailbox0, MBC_WRT_RAM_WORD);
	        FCWrite16(HostAdapterAddress + Mailbox1, Address);
#ifdef BIG_ENDIAN
	        {
	        	u16 temp = SWAP_16(*FirmwareBuffer);
	        	FCWrite16(HostAdapterAddress + Mailbox2, temp);
			}
#else
	        FCWrite16(HostAdapterAddress + Mailbox2, *FirmwareBuffer);
#endif
        SET_HOST2RISC_INTR(HostAdapterAddress);
        if (WaitForMboxCmdCmpltn(HostAdapter) != MB_STATUS_GOOD)
        	return FC_FALSE;
	        FirmwareLength--;
			Address++;
			FirmwareBuffer++;
	    }
	return FC_TRUE;

}

/* *******************************************************************************************
    LoadRiscSeqFirmware

    Description:
        Loads the RISC firmware into the RAM on the FC-2100 card

    Parameters:
        u16 *FirmwareBuffer:    Pointer to the 16 bit wide firmware buffer
        u16 FirmwareLength:     The length of the firmware buffer
        BaseAddressType HostAdapterAddress: Base address of HostAdapter

    Return Value:
        None

    Notes:

******************************************************************************************* */

Boolean LoadRiscSeqFirmware(u16 *SeqFwBuffer, u16 Length, u32 SeqStartAddress, HostAdapterType *HostAdapter)
{
	BaseAddressType BaseAddress = HostAdapter->BaseAddress;

    FCDEBUGPRINT(("\nLoading seq firmware via PIO\n"));
	    while (Length > 0)
	    {
	        WaitForMboxReady(BaseAddress);
	        FCWrite16(BaseAddress + Mailbox0, MBC_WRITE_RISC_WORD_EXT_BOOT);
	        FCWrite16(BaseAddress + Mailbox1, (SeqStartAddress & 0x0000ffff));
#ifdef BIG_ENDIAN
	        {
	        	u16 temp = SWAP_16(*SeqFwBuffer);
	        	FCWrite16(BaseAddress + Mailbox2, temp);
			}
#else
	        FCWrite16(BaseAddress + Mailbox2, *SeqFwBuffer);
#endif
	        FCWrite16(BaseAddress + Mailbox8, ((SeqStartAddress & 0xffff0000)>>16));
        SET_HOST2RISC_INTR(BaseAddress);
        if (WaitForMboxCmdCmpltn(HostAdapter) != MB_STATUS_GOOD)
    	return FC_FALSE;
	        Length--;
			SeqStartAddress++;
			SeqFwBuffer++;
	    }
	return FC_TRUE;
}

/* *******************************************************************************************
    DMARiscFirmware

    Description:
        Loads the RISC firmware into the RAM on the FC-2100 card

    Parameters:
        u32 Firmware_Buffer_PA:  Physical (PCI) Address 16 bit wide firmware buffer
        u16 FirmwareLength:     The length of the firmware buffer
        BaseAddressType HostAdapterAddress: Base address of HostAdapter

    Return Value:
        None                   

    Notes:

******************************************************************************************* */

u16 DMARiscFirmware(u32 Firmware_Buffer_PA, u16 FirmwareLength, HostAdapterType *HostAdapter)
{
		BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
        u32 Address = FIRMWARE_START_ADDRESS;

        FCDEBUGPRINT(("\nLoading firmware via DMA\n"));
        WaitForMboxReady(HostAdapterAddress);
#ifndef ISP24XX
        FCWrite16(HostAdapterAddress + Mailbox0, MBC_LOAD_RAM);
        FCWrite16(HostAdapterAddress + Mailbox1, Address);
        FCWrite16(HostAdapterAddress + Mailbox2, ((Firmware_Buffer_PA >>16) & 0xffff));
        FCWrite16(HostAdapterAddress + Mailbox3, ((Firmware_Buffer_PA     ) & 0xffff));
        FCWrite16(HostAdapterAddress + Mailbox4, FirmwareLength);
#else
        FCWrite16(HostAdapterAddress + Mailbox0, MBC_LOAD_RISC_RAM_EXT_BOOT);
        FCWrite16(HostAdapterAddress + Mailbox1, Address);
        FCWrite16(HostAdapterAddress + Mailbox2, ((Firmware_Buffer_PA >> 16) & 0xffff));
        FCWrite16(HostAdapterAddress + Mailbox3, ((Firmware_Buffer_PA      ) & 0xffff));
        FCWrite16(HostAdapterAddress + Mailbox4, 0);
        FCWrite16(HostAdapterAddress + Mailbox5, FirmwareLength);
        FCWrite16(HostAdapterAddress + Mailbox6, 0);
        FCWrite16(HostAdapterAddress + Mailbox7, 0);
        FCWrite16(HostAdapterAddress + Mailbox8, Address >> 16);
#endif
        SET_HOST2RISC_INTR(HostAdapterAddress);
        if (WaitForMboxCmdCmpltn(HostAdapter) != MB_STATUS_GOOD)
            return FC_FALSE;
        else
            return FC_TRUE;
}


/* *******************************************************************************************
    ReadRiscFirmware

    Description:
        Loads the RISC firmware into the RAM on the FC-2100 card

    Parameters:
        u32 Firmware_Buffer_PA:  Physical (PCI) Address 16 bit wide firmware buffer
        u16 FirmwareLength:     The length of the firmware buffer
        BaseAddressType HostAdapterAddress: Base address of HostAdapter

    Return Value:
        None                   

    Notes:

******************************************************************************************* */

u16 ReadRiscFirmware(u32 Firmware_Buffer_PA, u16 FirmwareLength, HostAdapterType *HostAdapter)
{
		BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
        u32 Address = FIRMWARE_START_ADDRESS;

        FCDEBUGPRINT(("\nLoading firmware via DMA\n"));
        WaitForMboxReady(HostAdapterAddress);
        FCWrite16(HostAdapterAddress + Mailbox0, MBC_DUMP_RAM);
        FCWrite16(HostAdapterAddress + Mailbox1, Address);
        FCWrite16(HostAdapterAddress + Mailbox2, ((Firmware_Buffer_PA >>16) & 0xffff));
        FCWrite16(HostAdapterAddress + Mailbox3, ((Firmware_Buffer_PA     ) & 0xffff));
        FCWrite16(HostAdapterAddress + Mailbox4, FirmwareLength);
        SET_HOST2RISC_INTR(HostAdapterAddress);
        if(WaitForMboxCmdCmpltn(HostAdapter) != MB_STATUS_GOOD)
            return FC_FALSE;
        else
            return FC_TRUE;
}



/************************************************************************
*
*  Verify RISC Firmware Checksum.
*
*************************************************************************/

Boolean VerifyRiscFwChecksum(HostAdapterType *HostAdapter)
{
    u32 Address = FIRMWARE_START_ADDRESS;


	BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    FCDEBUGPRINT(("Verifying firmware checksum \n"));
    WaitForMboxReady(HostAdapterAddress);

#ifndef ISP24XX
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_VER_CHKSUM);
    FCWrite16(HostAdapterAddress + Mailbox1, Address & 0xFFFF);
#else
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_VER_CHKSUM);
    FCWrite16(HostAdapterAddress + Mailbox1, Address >> 16);
    FCWrite16(HostAdapterAddress + Mailbox2, Address & 0xFFFF);
#endif

    SET_HOST2RISC_INTR(HostAdapterAddress);
    if(WaitForMboxCmdCmpltn(HostAdapter) != MB_STATUS_GOOD)
        return FC_FALSE;
    else
        return FC_TRUE;
}


/************************************************************************
*
*  Execute RISC Firmware.
*
*************************************************************************/

u16 ExecuteRiscFirmware(HostAdapterType *HostAdapter)
{
    u32 Address = FIRMWARE_START_ADDRESS;


	BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
	u16 Status;
    WaitForMboxReady(HostAdapterAddress);
    
#ifndef ISP24XX
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_EXE_FW);
    FCWrite16(HostAdapterAddress + Mailbox1, Address & 0xFFFF);
    FCWrite16(HostAdapterAddress + Mailbox2, 0);
#else
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_EXE_FW);
    FCWrite16(HostAdapterAddress + Mailbox1, Address >> 16);
    FCWrite16(HostAdapterAddress + Mailbox2, Address & 0xFFFF);
#endif

    SET_HOST2RISC_INTR(HostAdapterAddress);
#if 0
	Status = WaitForMboxCmdCmpltn(HostAdapter);
#else
	{
		volatile u32 Counter = 10;
		while (Counter>0)
		{
			Status = WaitForMbCmdCmpltServiceAEs(HostAdapter);
			if (Status == 0)	Counter--;
			else return Status;
		}
	}
#endif
	return (Status);
}


/************************************************************************
*
*  Initialize RISC Firmware.
*      Starts Request and Response Queues.
*
************************************************************************/

u16 InitializeFirmware(HostAdapterType *HostAdapter)
{
    tIFWControlBlock *IFWCB;
    u16 MBStatus;
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;

    IFWCB = (tIFWControlBlock *) HostAdapter->ReqQAddress;     /* ok to use Request Queue now since not started */
    /* Build Initialization Control Block */

    IFWCB->Version       = 1;
    IFWCB->DelphiOptions = 0;
    IFWCB->FrmLength     = HostAdapter->FrameSize;
    IFWCB->MaxResourceAlloc = MAX_RESOURCE_ALLOCATION;
    IFWCB->Throttle      = THROTTLE_COUNT;
    IFWCB->RetryCount    = RETRY_COUNT;
    IFWCB->RetryDelay    = RETRY_DELAY;

    if(HostAdapter->Flags.FastPostEnabled)
        IFWCB->Options   = FW_INIT_OPTIONS | FAST_POST_OPTION;
    else
        IFWCB->Options   = FW_INIT_OPTIONS;

    if(HostAdapter->Options & PORT_DB_CHG_NOTIF_ENABLE_FLAG)
        IFWCB->Options   |= PORT_DB_CHG_NOTIF_OPTION;

#ifdef ENABLE_AMCD
    if(HostAdapter->Options & FAIRNESS_DISABLE_FLAG)
        IFWCB->Options   &=  FAIRNESS_DISABLE_OPTION;
    if(HostAdapter->Options & FORCE_LOGIN_FLAG)
        IFWCB->Options   |=  FORCE_LOGIN_OPTION;
#else
    if (HostAdapter->Options & IMPLICIT_LIP_ENABLE_FLAG)
        IFWCB->DelphiOptions |= DEL_OPT_IMPLICIT_LIP;

    if (HostAdapter->Options & IMPLICIT_LOGIN_ENABLE_FLAG)
        IFWCB->DelphiOptions |= DEL_OPT_IMPLICIT_LOGIN;
#endif


    IFWCB->PortName0     = HostAdapter->WorldWideName.Bytes[0];
    IFWCB->PortName1     = HostAdapter->WorldWideName.Bytes[1];
    IFWCB->PortName2     = HostAdapter->WorldWideName.Bytes[2];
    IFWCB->PortName3     = HostAdapter->WorldWideName.Bytes[3];
    IFWCB->PortName4     = HostAdapter->WorldWideName.Bytes[4];
    IFWCB->PortName5     = HostAdapter->WorldWideName.Bytes[5];
    IFWCB->PortName6     = HostAdapter->WorldWideName.Bytes[6];
    IFWCB->PortName7     = HostAdapter->WorldWideName.Bytes[7];

    IFWCB->HardAddress   = HostAdapter->PortID;
    IFWCB->Rsvd0         = 0;
    if(HostAdapter->Flags.NodeNameEnabled)
    {
        IFWCB->Options   |= NODE_NAME_ENABLED_OPTION;
        
        IFWCB->NodeName0     = HostAdapter->NodeName.Bytes[0];
        IFWCB->NodeName1     = HostAdapter->NodeName.Bytes[1];
        IFWCB->NodeName2     = HostAdapter->NodeName.Bytes[2];
        IFWCB->NodeName3     = HostAdapter->NodeName.Bytes[3];
        IFWCB->NodeName4     = HostAdapter->NodeName.Bytes[4];
        IFWCB->NodeName5     = HostAdapter->NodeName.Bytes[5];
        IFWCB->NodeName6     = HostAdapter->NodeName.Bytes[6];
        IFWCB->NodeName7     = HostAdapter->NodeName.Bytes[7];
    }
    else
    {
        IFWCB->NodeName0     = HostAdapter->WorldWideName.Bytes[0];
        IFWCB->NodeName1     = HostAdapter->WorldWideName.Bytes[1];
        IFWCB->NodeName2     = HostAdapter->WorldWideName.Bytes[2];
        IFWCB->NodeName3     = HostAdapter->WorldWideName.Bytes[3];
        IFWCB->NodeName4     = HostAdapter->WorldWideName.Bytes[4];
        IFWCB->NodeName5     = HostAdapter->WorldWideName.Bytes[5];
        IFWCB->NodeName6     = HostAdapter->WorldWideName.Bytes[6];
        IFWCB->NodeName7     = HostAdapter->WorldWideName.Bytes[7];
    }
    IFWCB->ReqQOutIndex  = 0;
    IFWCB->RspQInIndex   = 0;
    IFWCB->ReqQLength    = REQ_RESP_QUEUE_NUM_ENTRIES;
    IFWCB->RspQLength    = REQ_RESP_QUEUE_NUM_ENTRIES;
    IFWCB->ReqQAddress0  = HostAdapter->ReqQPA;
    IFWCB->ReqQAddress1  = 0L;
    IFWCB->RspQAddress0  = HostAdapter->RespQPA;
    IFWCB->RspQAddress1  = 0L;
    IFWCB->HardAddress <<= 8;
    IFWCB->LunEnables    = 1;
    IFWCB->CommandRsrcCount = 0xFF;
    IFWCB->ImmedNotifyCount = 0xFF;

    IFWCB->Timeout       = 0;
    IFWCB->ReservedA     = 0;

    IFWCB->AdditFWOptions= 0;
    IFWCB->SpecialOptions= 0;
    if (HostAdapter->Options & POINT_TO_POINT_ENABLE_FLAG)
        IFWCB->AdditFWOptions |= 0x0010;
    else if (HostAdapter->Options & ARBITRATED_LOOP_PREFERRED_ELSE_P2P)
        IFWCB->AdditFWOptions |= 0x0020;

    if (HostAdapter->Options & CLASS2_ENABLE_FLAG)
        IFWCB->AdditFWOptions |= 0x100;
    if (HostAdapter->Options & ACK0_ENABLE_FLAG)
        IFWCB->AdditFWOptions |= 0x200;
    IFWCB->RespAccTimer  = 0;
    IFWCB->IntDelayTimer = 0;

    if (HostAdapter->Options & AUTO_2_GIG)
        IFWCB->SpecialOptions |= 0x8000; 
    else if (HostAdapter->Options & FIXED_2_GIG)
        IFWCB->SpecialOptions |= 0x4000;
        
    if (HostAdapter->Options & FIFTY_OHM_TERMINATION_ENABLE_FLAG)
        IFWCB->SpecialOptions |= 0x2000; 

    if (HostAdapter->Options & DISABLE_AUTO_LOGIN_ON_LIP)
        IFWCB->SpecialOptions |= 0x0080;

#ifdef ENABLE_AMCD
    if (HostAdapter->Options & USE_GPIO_EN_FLAG)
        IFWCB->SpecialOptions |= 0x0001; 
    else if (HostAdapter->Options & FIFO_SNOOP_EN_FLAG)
        IFWCB->SpecialOptions |= 0x0002;
#endif    
    IFWCB->Rsvd5         = 0;

    IFWCB->Rsvd6         = 0;
    IFWCB->Rsvd7         = 0;
    IFWCB->Rsvd8         = 0;
    IFWCB->Rsvd9         = 0;
    IFWCB->Rsvd10        = 0;
    IFWCB->Rsvd11        = 0;

if (HostAdapter->DevType >= FD_ISP_2300)
{
    FCWrite16(HostAdapterAddress+IspReqInPtr, 0);           /* make sure RESP and REQ queue pointers are reset */
    FCWrite16(HostAdapterAddress+IspRespQOutPtr, 0);
}

#ifdef BIG_ENDIAN
    BigEndConvert((u32 *)IFWCB, IFWCBCWORDS, IFWCBSENSE, IFWCBCDB, FC_FALSE);
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
   Fibre_Flush_Cache((u8*) IFWCB, sizeof(*IFWCB));
#endif

FCDEBUGPRINT(("Starting Initialization\n"));
    if (WaitForMboxReady(HostAdapterAddress))
    	return MB_STATUS_INTERFACE_ERROR;
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_INIT_FW);
    FCWrite16(HostAdapterAddress + Mailbox2, (u16)(HostAdapter->ReqQPA >> 16));
    FCWrite16(HostAdapterAddress + Mailbox3, (u16)(HostAdapter->ReqQPA & 0xffff));
    FCWrite16(HostAdapterAddress + Mailbox4, 0);
    FCWrite16(HostAdapterAddress + Mailbox5, 0);
    FCWrite16(HostAdapterAddress + Mailbox6, 0);
    FCWrite16(HostAdapterAddress + Mailbox7, 0);
    SET_HOST2RISC_INTR(HostAdapterAddress);
FCDEBUGPRINT(("Sent Initialization command\n"));
	{
		volatile u32 Counter = 10;
		while (Counter>0)
		{
			MBStatus = WaitForMbCmdCmpltServiceAEs(HostAdapter);
			if (MBStatus == 0)	Counter--;
			else return MBStatus;
		}
	}
   return MBStatus;

}




/************************************************************************
*
*  Read/Write RISC RAM
*
************************************************************************/

void RISC_Access (u16 RISCAddress, s16 TransferDirection, u16 *Data, HostAdapterType *HostAdapter)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 MbStatus;
    switch ( TransferDirection )
    {
    case RISC_READ:
        WaitForMboxReady(HostAdapterAddress);
        FCWrite16(HostAdapterAddress + Mailbox0, MBC_RD_RAM_WORD);
        FCWrite16(HostAdapterAddress + Mailbox1, RISCAddress);
        SET_HOST2RISC_INTR(HostAdapterAddress);
        MbStatus = WaitForMboxCmdCmpltn(HostAdapter);
        *Data = FCRead16(HostAdapterAddress + Mailbox2);
        break;

    case RISC_WRITE:
        WaitForMboxReady(HostAdapterAddress);
        FCWrite16(HostAdapterAddress + Mailbox0, MBC_WRT_RAM_WORD);
        FCWrite16(HostAdapterAddress + Mailbox1, RISCAddress);
        FCWrite16(HostAdapterAddress + Mailbox2, *Data);
        SET_HOST2RISC_INTR(HostAdapterAddress);
        WaitForMboxCmdCmpltn(HostAdapter);
        break;

    default:
        break;
    }
}

/************************************************************************
*
*  Read/Write RISC RAM
*
************************************************************************/

void RISC_Ext_Access (u32 RISCAddress, s16 TransferDirection, u16 *Data, HostAdapterType *HostAdapter)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 MbStatus;
    switch ( TransferDirection )
    {
    case RISC_READ:
        WaitForMboxReady(HostAdapterAddress);
        FCWrite16(HostAdapterAddress + Mailbox0, MBC_READ_RISC_WORD_EXT_BOOT);
        FCWrite16(HostAdapterAddress + Mailbox1, (RISCAddress & 0x0000ffff));
        FCWrite16(HostAdapterAddress + Mailbox8, ((RISCAddress & 0xffff0000)>>16));
        SET_HOST2RISC_INTR(HostAdapterAddress);
        MbStatus = WaitForMboxCmdCmpltn(HostAdapter);
        *Data = FCRead16(HostAdapterAddress + Mailbox2);
        break;

    case RISC_WRITE:
        WaitForMboxReady(HostAdapterAddress);
        FCWrite16(HostAdapterAddress + Mailbox0, MBC_WRITE_RISC_WORD_EXT_BOOT);
        FCWrite16(HostAdapterAddress + Mailbox1, (RISCAddress & 0x0000ffff));
        FCWrite16(HostAdapterAddress + Mailbox2, *Data);
        FCWrite16(HostAdapterAddress + Mailbox8, ((RISCAddress & 0xffff0000)>>16));
        SET_HOST2RISC_INTR(HostAdapterAddress);
        WaitForMboxCmdCmpltn(HostAdapter);
        break;

    default:
        break;
    }
}

/************************************************************************
*
*  Initialize RISC RAM
*
*  Sets currently selelcted Risc Ram bank to Value.  Does this from
*  Start Address and up for Length words.
************************************************************************/

void InitRiscRam(u16 StartAddress, u16 Length, u16 Value, HostAdapterType *HostAdapter)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 Temp;

    WaitForMboxReady(HostAdapterAddress);
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_INIT_RISC_RAM_BOOT);
    FCWrite16(HostAdapterAddress + Mailbox1, StartAddress);
    FCWrite16(HostAdapterAddress + Mailbox2, Length);
    FCWrite16(HostAdapterAddress + Mailbox3, Value);
    SET_HOST2RISC_INTR(HostAdapterAddress);
    Temp = WaitForMboxCmdCmpltn(HostAdapter);
}

/************************************************************************
*
*  Load RISC RAM Extended - for 2300 and higher
*
*  loads RISC RAM from system memory.  Does this from
*  RISCAddress and up for Length words.  Must be within a 64K block
************************************************************************/

void LoadRiscRamExtended(u32 RISCAddress, u32 SourceAddressPA, u16 Length,HostAdapterType *HostAdapter)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 Temp;

    WaitForMboxReady(HostAdapterAddress);
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_LOAD_RISC_RAM_EXT_BOOT);
    FCWrite16(HostAdapterAddress + Mailbox1, RISCAddress);
    FCWrite16(HostAdapterAddress + Mailbox2, SourceAddressPA >>16);
    FCWrite16(HostAdapterAddress + Mailbox3, SourceAddressPA);
    FCWrite16(HostAdapterAddress + Mailbox4, Length);
    FCWrite16(HostAdapterAddress + Mailbox6, 0);
    FCWrite16(HostAdapterAddress + Mailbox7, 0);
    FCWrite16(HostAdapterAddress + Mailbox8, RISCAddress >> 16);

    SET_HOST2RISC_INTR(HostAdapterAddress);
    Temp = WaitForMboxCmdCmpltn(HostAdapter);
}

/************************************************************************
*
*  Dump RISC RAM Extended - for 2300 and higher
*
*  Reads RISC RAM into system memory.  Does this from
*  RISCAddress and up for Length words.  Must be within a 64K block
************************************************************************/

void DumpRiscRamExtended(u32 RISCAddress, u32 DestAddressPA, u16 Length, HostAdapterType *HostAdapter)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 Temp;

    WaitForMboxReady(HostAdapterAddress);
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_DUMP_RISC_RAM_EXT_BOOT);
    FCWrite16(HostAdapterAddress + Mailbox1, RISCAddress);
    FCWrite16(HostAdapterAddress + Mailbox2, DestAddressPA >>16);
    FCWrite16(HostAdapterAddress + Mailbox3, DestAddressPA);
    FCWrite16(HostAdapterAddress + Mailbox4, Length);
    FCWrite16(HostAdapterAddress + Mailbox6, 0);
    FCWrite16(HostAdapterAddress + Mailbox7, 0);
    FCWrite16(HostAdapterAddress + Mailbox8, RISCAddress >> 16);

    SET_HOST2RISC_INTR(HostAdapterAddress);
    Temp = WaitForMboxCmdCmpltn(HostAdapter);
}


/************************************************************************
*
*  Write RISC RAM Extended - for 2300 and higher
*
*  Writes the 16 bit value to the specified address
************************************************************************/

void WriteRiscRamExtended(u32 RISCAddress, u16 Data, HostAdapterType *HostAdapter)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 Temp;
    WaitForMboxReady(HostAdapterAddress);
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_WRITE_RISC_WORD_EXT_BOOT);
    FCWrite16(HostAdapterAddress + Mailbox1, RISCAddress);
    FCWrite16(HostAdapterAddress + Mailbox2, Data);
    FCWrite16(HostAdapterAddress + Mailbox8, RISCAddress >> 16);

    SET_HOST2RISC_INTR(HostAdapterAddress);
    Temp = WaitForMboxCmdCmpltn(HostAdapter);
}

/************************************************************************
*
*  Read RISC RAM Extended - for 2300 and higher
*
*  Read the 16 bit value from the specified address
************************************************************************/

u16 ReadRiscRamExtended(u32 RISCAddress, HostAdapterType *HostAdapter)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 Temp;

    WaitForMboxReady(HostAdapterAddress);
    FCWrite16(HostAdapterAddress + Mailbox0, MBC_READ_RISC_WORD_EXT_BOOT);
    FCWrite16(HostAdapterAddress + Mailbox1, RISCAddress);
    FCWrite16(HostAdapterAddress + Mailbox8, RISCAddress >> 16);

    SET_HOST2RISC_INTR(HostAdapterAddress);
    WaitForMboxCmdCmpltn(HostAdapter);
    Temp = FCRead16(HostAdapterAddress + Mailbox0);
    return (FCRead16(HostAdapterAddress + Mailbox2));
}

/************************************************************************
*
*  Wait for FC-2100 Mailbox Ready.
*
************************************************************************/
#ifndef ISP24XX
s16 WaitForMboxReady(BaseAddressType HostAdapterAddress)
{
    u16 semaphore,sis,hcc;

    volatile u32 Counter = 1000 * FC2100ResetDelay;

    semaphore = FCRead16(HostAdapterAddress + IspSemaphore);
    if(semaphore & 0x0001)
    {
        /* clear semaphore */
        FCWrite16(HostAdapterAddress + IspSemaphore, 0);
    }
    do
    {
        sis = FCRead16(HostAdapterAddress + Isp2PciIntSts);
    } while (FCRead16(HostAdapterAddress + Isp2PciIntSts) != sis);

    if(sis & RISC_INT_PENDING)
    {
        /* clear RISC interrupt to host */
        CLEAR_RISC2HOST_INTR(HostAdapterAddress);
    }
    hcc = FCRead16(HostAdapterAddress + IspHccr);
    while((hcc & HCTLH2RINTR) && (Counter != 0))
    {
        /* wait for ISP to clear host interrupt to RISC */
        hcc = FCRead16(HostAdapterAddress + IspHccr);
        Counter--;
    }
    if (Counter == 0)
        return(1);
    else
        return(0);
}
#else /*ISP24XX*/
s16 WaitForMboxReady(BaseAddressType HostAdapterAddress)
{
    u16 sis;
    u32 hcc;

    volatile u32 Counter = 1000 * FC2100ResetDelay;

    do
    {
        sis = FCRead32(HostAdapterAddress + Isp2PciIntSts);
    } while (FCRead32(HostAdapterAddress + Isp2PciIntSts) != sis);

    if(sis & RISC_INT_PENDING)
    {
        /* clear RISC interrupt to host */
        CLEAR_RISC2HOST_INTR(HostAdapterAddress);
    }
    hcc = FCRead32(HostAdapterAddress + IspHccr);
    while((hcc & HCTLH2RINTR) && (Counter != 0))
    {
        /* wait for ISP to clear host interrupt to RISC */
        hcc = FCRead32(HostAdapterAddress + IspHccr);
        Counter--;
    }
    if (Counter == 0)
        return(1);
    else
        return(0);
}
#endif
/************************************************************************
*
*  Wait for ISP2100 Mailbox Command Completion.
*
************************************************************************/

u16 WaitForMboxCmdCmpltn(HostAdapterType *HostAdapter)
{
    u32 ISPInterruptStatus;
    u16 MB0;
    volatile u32 counter = 10000 * FC2100ResetDelay;
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
#ifndef ISP24XX
    if (HostAdapter->DevType < FD_ISP_2300)
	{
		ISPInterruptStatus = FCRead16(HostAdapterAddress + Isp2PciIntSts);
		while (((ISPInterruptStatus & RISC_INT_PENDING)==0) && (counter !=0))
		{
			ISPInterruptStatus = FCRead16(HostAdapterAddress + Isp2PciIntSts);
	        counter--;
		}
		if (counter == 0)
		{
			FCDEBUGPRINT(("Timed out waiting for MB completion!!!\n"));
			return (0);
        }
        MB0 = FCRead16(HostAdapterAddress + Mailbox0);
        FCWrite16( HostAdapterAddress + IspSemaphore, SEMAPHORE_UNLOCK);
	}
	else
#endif
	{
		ISPInterruptStatus = FCRead32(HostAdapterAddress + IspStatus);
		while (((ISPInterruptStatus & ISP23xx_RISC_INT_PENDING)==0) && (counter !=0))
		{
			ISPInterruptStatus = FCRead32(HostAdapterAddress + IspStatus);
	        counter--;
		}
		if (counter == 0)
		{
			FCDEBUGPRINT(("Timed out waiting for MB completion!!!\n"));
			return (0);
        }
        MB0 = ISPInterruptStatus >> 16;  /* saves one PCI bus access since info is in upper 16 bits of 32 status on 23xx */
	}
    CLEAR_RISC2HOST_INTR(HostAdapterAddress);    /* clear RISC interrupt to host */
	return MB0;
}
u16 WaitForMbCmdCmpltServiceAEs(HostAdapterType *HostAdapter)
{
    u32 ISPInterruptStatus;
	u16 MBStatus;
    volatile u32 Counter = 10000 * FC2100ResetDelay;
	Boolean ProcessInt = FC_FALSE;
	BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
#ifndef ISP24XX
    if (HostAdapter->DevType < FD_ISP_2300)
	{
		ISPInterruptStatus = FCRead16(HostAdapterAddress + Isp2PciIntSts);
		while (((ISPInterruptStatus & RISC_INT_PENDING)== 0) && (Counter !=0))
		{
			ISPInterruptStatus = FCRead16(HostAdapterAddress + Isp2PciIntSts);
	        Counter--;
		}
		if (Counter == 0)
			ProcessInt = FC_FALSE;
		if (FCRead16(HostAdapterAddress + IspSemaphore) & SEMAPHORE_LOCK) /* then MBC complete or Async event*/
			ProcessInt = FC_TRUE;
	}
	else
#endif
	{
		ISPInterruptStatus = FCRead32(HostAdapterAddress + IspStatus);
		while (((ISPInterruptStatus & ISP23xx_RISC_INT_PENDING) == 0) && (Counter !=0))
		{
			ISPInterruptStatus = FCRead32(HostAdapterAddress + IspStatus);
	        Counter--;
		}
		if (Counter == 0)
			ProcessInt = FC_FALSE;
		if ((ISPInterruptStatus & 0xFF) != 0x13)  /* then MBC complete or Async event*/ 
			ProcessInt = FC_TRUE;
	}
	if (ProcessInt)
	{
		if (HostAdapter->DevType < FD_ISP_2300)
        {
            MBStatus = FCRead16(HostAdapterAddress + Mailbox0);
			if ((MBStatus & 0xF000) != MB_STATUS_GOOD)
				HandleAsyncEvent(HostAdapter, MBStatus);
			else return MBStatus;
		}
		else
		{
            if ((ISPInterruptStatus & 0xFF) >= 0x16) /* then fast post completion and need to read MB0 */
                MBStatus = FCRead16(HostAdapterAddress + Mailbox0);
            else
                MBStatus = ISPInterruptStatus >> 16;  /* saves one PCI bus access since info is in upper 16 bits of 32 status on 23xx */
			if ((MBStatus & 0xF000) != MB_STATUS_GOOD)
				HandleAsyncEvent(HostAdapter, MBStatus);
			else return MBStatus;
		}
	}
	MBStatus = 0;
	return MBStatus; 
}

/************************************************************************
*
*  Service RISC to Host Interrupt.
*
************************************************************************/

Boolean ServiceRiscInterrupt(HostAdapterType *HostAdapter)
{
    u16 MB0,MB1,MB2,MB3,MB6,MB7;
    u32 ISPInterruptStatus;
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 i;
    Boolean IntActive = FC_FALSE, ProcessInt = FC_FALSE;
#ifndef ISP24XX    
    if (HostAdapter->DevType < FD_ISP_2300)
	{
		ISPInterruptStatus = FCRead16(HostAdapterAddress + Isp2PciIntSts);
		if (ISPInterruptStatus & RISC_INT_PENDING)
	        IntActive = FC_TRUE;
		else
			return (IntActive);
		if (FCRead16(HostAdapterAddress + IspSemaphore) & SEMAPHORE_LOCK) /* then MBC complete or Async event*/
			ProcessInt = FC_TRUE;
	}
	else
#endif
	{
		ISPInterruptStatus = FCRead32(HostAdapterAddress + IspStatus);
		if (ISPInterruptStatus & ISP23xx_RISC_INT_PENDING)
	        IntActive = FC_TRUE;
		else
			return (IntActive);
		if ((ISPInterruptStatus & 0xFF) != 0x13)  /* then MBC complete or Async event*/ 
			ProcessInt = FC_TRUE;
	}
	
	if (ProcessInt)   
	{

			if (HostAdapter->DevType < FD_ISP_2300)
	            MB0 = FCRead16(HostAdapterAddress + Mailbox0);
			else
			{
	            if ((ISPInterruptStatus & 0xFF) >= 0x16) /* then fast post completion and need to read MB0 */
	                MB0 = FCRead16(HostAdapterAddress + Mailbox0);
	            else
	                MB0 = ISPInterruptStatus >> 16;  /* saves one PCI bus access since info is in upper 16 bits of 32 status on 23xx */
			}
            if ((MB0 & MB_STATUS_GOOD) == MB_STATUS_GOOD)              /* if mailbox command complete */
            {
                HostAdapter->MB0 = MB0;

                switch  (HostAdapter->MBCState)
                {
                    
                    case PortMapRequestSent:                                /* then this must be its completion notice  */
    
                        if (!ProcessPortMapResponse(HostAdapter))           /* if not a good response then try again    */
                            HostAdapter->Flags.RequestPortMap = FC_TRUE;
                        else
                            HostAdapter->FibreStatus.PortMapValid = FC_TRUE;
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case GetLoopIDSent:                   
    
                        if (MB0 == MB_STATUS_GOOD)             /* process get loop ID info to see if Nport to Nport */
                        {
        
                            MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                            MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                            MB3 = FCRead16(HostAdapterAddress + Mailbox3);
                            MB6 = FCRead16(HostAdapterAddress + Mailbox6);
            
                            HostAdapter->FibreStatus.LoopID = MB1;
                            HostAdapter->FibreStatus.PortID = MB2 | (MB3 << 16);
                            HostAdapter->FibreStatus.IDValid= FC_TRUE;
                            HostAdapter->FibreStatus.ConnectionType = MB6 + FCCT_PRIVATE_LOOP;
                            if (HostAdapter->StatusHandler)
                                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_PORT_ID, HostAdapter->FibreStatus.PortID);
    
                            if (MB6 == 2) /* check if P2P no switch */
                            {
                                if (MB1 !=0)                               /* if not ID 0 then need to login to 0  */     
                                {
                                    HostAdapter->Flags.RequestLoopPortLogin = FC_TRUE;
                                }
                            }
                        } 
                        else
                            HostAdapter->Flags.RequestLoopID = FC_TRUE;    /* re-request loop ID */
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case LoopPortLoginSent:                   
    
                        if (MB0 != MB_STATUS_GOOD)                         /* verify logged in    */
                            HostAdapter->Flags.RequestLoopPortLogin = FC_TRUE;          /* re-request login    */
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case ForceLIPSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);            /* status 0 == GOOD */
                        else
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);            /* status 1 == BAD  */
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                    
                    case OmniPortControlSent:
                    
                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(MB1 << 8, HostAdapter->MBCClientID);  /* status 0 == GOOD */
                        else
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);            /* status 1 == BAD  */
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case IDAliasSent:
    
                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                        if (MB0 == MB_STATUS_GOOD)
                           HostAdapter->MBCCallBackFunction(HostAdapter->FabricTargetPortLoopIDAlias, HostAdapter->MBCClientID);
                        else if (MB0 == 0x4007)
                            HostAdapter->MBCCallBackFunction(MB1, HostAdapter->MBCClientID);  /* already assinged ID */
                        else if (MB0 == 0x4005)
                        {
						   u16 ret_val = ALIAS_NO_LINK+MB1;
						   if (ret_val >= ALIAS_ERR) ret_val = ALIAS_ERR;  /* cannot return value greater than enum type */	
                           HostAdapter->MBCCallBackFunction((ret_val << 16) | 0xFFFF, HostAdapter->MBCClientID);    /* Parameter error */
                        }
                        else if (MB0 == 0x4006)
                            HostAdapter->MBCCallBackFunction((ALIAS_COMMAND_PARAM_ERROR << 16) | 0xFFFF, HostAdapter->MBCClientID);    /* Parameter error */
                        else if (MB0 == 0x4008)
                            HostAdapter->MBCCallBackFunction((ALIAS_LOOPID_USED << 16) | 0xFFFF, HostAdapter->MBCClientID);    /* Parameter error */
                        else if (MB0 == 0x4009)
                            HostAdapter->MBCCallBackFunction((ALIAS_ALL_LOOPIDS_USED << 16) | 0xFFFF, HostAdapter->MBCClientID);    /* Parameter error */
                        else    
                            HostAdapter->MBCCallBackFunction((ALIAS_NON_SPECIFIC_ERROR << 16) | 0xFFFF, HostAdapter->MBCClientID);    /* Parameter error */
                            
                         HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case SNSSent:
                    
                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else 
                            HostAdapter->MBCCallBackFunction(MB1, HostAdapter->MBCClientID);  
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case NextSNSSent:
                    
                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else 
                            HostAdapter->MBCCallBackFunction(MB1, HostAdapter->MBCClientID);  
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case GetWWNSent:

                        MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                        MB3 = FCRead16(HostAdapterAddress + Mailbox3);
                        MB6 = FCRead16(HostAdapterAddress + Mailbox6);
                        MB7 = FCRead16(HostAdapterAddress + Mailbox7);
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->WWNPtr->Bytes[0] = MB7 >> 8;
                            HostAdapter->WWNPtr->Bytes[1] = MB7 & 0xFF;
                            HostAdapter->WWNPtr->Bytes[2] = MB6 >> 8;
                            HostAdapter->WWNPtr->Bytes[3] = MB6 & 0xFF;
                            HostAdapter->WWNPtr->Bytes[4] = MB3 >> 8;
                            HostAdapter->WWNPtr->Bytes[5] = MB3 & 0xFF;
                            HostAdapter->WWNPtr->Bytes[6] = MB2 >> 8;
                            HostAdapter->WWNPtr->Bytes[7] = MB2 & 0xFF;
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else if (MB0 == MB_STATUS_COMMAND_ERROR)
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);
                        else 
                            HostAdapter->MBCCallBackFunction(2, HostAdapter->MBCClientID);
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                    
                    case GetNameIdListSent:

                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);  /* get number of bytes xferd */
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            u16 numPorts = MB1/32;
#ifdef BIG_ENDIAN
                            u16 numLwords = MB1/4;
#endif
                            u8 i;

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
                            Fibre_Invalidate_Cache((u8*) HostAdapter->MBCommandBuffer, MB1);
#endif

#ifdef BIG_ENDIAN
                            BigEndConvert((u32 *)HostAdapter->MBCommandBuffer, numLwords, 0,0,0);                           
#endif
                            for (i=0; i<numPorts; i++)
                            {
                                *(HostAdapter->MBSNameIdListPointer+i) = *(((NameIdListType*)HostAdapter->MBCommandBuffer)+i);
                            }
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID, numPorts);
                        }
                        else if (MB0 == MB_STATUS_COMMAND_ERROR)
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID, 0);
                        else 
                            HostAdapter->MBCCallBackFunction(2, HostAdapter->MBCClientID, 0);
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                    
                    case SetRDMABaseSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                        {
                             HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->RDMANotification = FC_NULL;                           /* error so remove pointer */
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case SetRDMASubAddrSent:

                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->RDMANotification = FC_NULL;                           /* error so remove pointer */
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case SetRDMAMultipleSent:

                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->RDMANotification = FC_NULL;                           /* error so remove pointer */
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case GetSubAddrSingleSent:

                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                        MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                        MB3 = FCRead16(HostAdapterAddress + Mailbox3);
                        MB6 = FCRead16(HostAdapterAddress + Mailbox6);
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID, MB1, MB2, MB3, MB6);
                        }
                        else
                        {
                            HostAdapter->RDMANotification = FC_NULL;                           /* error so remove pointer */
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID,0,0,0,0);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case GetSubAddrListSent:

                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->RDMANotification = FC_NULL;                           /* error so remove pointer */
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case GetDataRateSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                        {
                             MB1 = FCRead16(HostAdapterAddress + Mailbox1);  /* MB1 contains data rate */
                             HostAdapter->MBCCallBackFunction(MB1, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->MBCCallBackFunction(MB0, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case ImplicitPortSent:  
                    
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                            HostAdapter->Flags.RequestPortMap = FC_TRUE;          /* request new map since we don't get lip     */
                            for (i = 0; i < PORT_MAP_SIZE; i++)
                                HostAdapter->FibreStatus.PortMap[i] = FC_FALSE;   /* invalidate Port Map until we get new map   */
                            HostAdapter->FibreStatus.PortMapValid = FC_FALSE;
                        }        
                        else
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);            /* status 1 == BAD  */
    
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                    
                    case IPInitSent:  
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->IPInitialized = FC_TRUE;
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);            /* status 1 == BAD  */
   
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                   
                    case SetIPFastBufSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                    
                    case EnableWDCSent:
                    
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case GenericMBCSent:
                    
                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                        MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                        MB3 = FCRead16(HostAdapterAddress + Mailbox3);
                        MB6 = FCRead16(HostAdapterAddress + Mailbox6);
                        MB7 = FCRead16(HostAdapterAddress + Mailbox7);
                        HostAdapter->MBCCallBackFunction(MB0, MB1, MB2, MB3, MB6, MB7);
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case LoopBackTestSent:
    
                        MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                        MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                        MB3 = FCRead16(HostAdapterAddress + Mailbox3);
						FCDEBUGPRINT(("MB0 = %4x MB1 = %4x MB2 = %4x MB3 = %4x\n", MB0, MB1, MB2, MB3));
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else if (MB0 == MB_STATUS_LOOPBACK_ERR)
                        {
                            HostAdapter->MBCCallBackFunction(2, HostAdapter->MBCClientID);    
                        }
                        else if (MB0 == MB_STATUS_LOOPUP_REQUIRED)
                        {
                            HostAdapter->MBCCallBackFunction(3, HostAdapter->MBCClientID);    
                        }
                         else
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case DiagnosticEchoSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else if (MB0 == MB_STATUS_LOOPBACK_ERR)
                        {
                            HostAdapter->MBCCallBackFunction(2, HostAdapter->MBCClientID);    
                        }
                        else if (MB0 == MB_STATUS_LOOPUP_REQUIRED)
                        {
                            HostAdapter->MBCCallBackFunction(3, HostAdapter->MBCClientID);    
                        }
                         else
                        {
                            HostAdapter->MBCCallBackFunction(MB0, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                                                
                    case PortAbortRequestSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            SendMarker(HostAdapter, MKR_Mod_IT);
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else if (MB0 == MB_STATUS_PARAM_ERROR)
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        else
                        {
                            HostAdapter->MBCCallBackFunction(2, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case DriverHBSent:
    
                        if (MB0 == MB_STATUS_PARAM_ERROR)
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        else
                        {
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case FirmwareHBSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case BusResetSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            SendMarker(HostAdapter, MKR_Mod_ALL);
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                        
                    case LUNResetSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            SendMarker(HostAdapter, MKR_Mod_ITL);
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else if (MB0 == MB_STATUS_PARAM_ERROR)
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                         else
                        {
                            HostAdapter->MBCCallBackFunction(2, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case AbortInitiatorSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        else if (MB0 == MB_STATUS_PARAM_ERROR)
                        {
                            HostAdapter->MBCCallBackFunction(2, HostAdapter->MBCClientID);    
                        }
                         else
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
                        
                    case GetLESBSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                        {
              
                            #ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
                                Fibre_Invalidate_Cache((u8 *)HostAdapter->MBCommandBuffer, sizeof(LESBType));
                            #endif

                            #ifdef BIG_ENDIAN
                                BigEndConvert((u32 *)HostAdapter->MBCommandBuffer, sizeof(LESBType)/4, 0, 0, FC_FALSE);
                            #endif
                            *HostAdapter->MBCUserLESBPointer = *((LESBType*)HostAdapter->MBCommandBuffer);
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case ResetLESBSent:
    
                        if (MB0 == MB_STATUS_GOOD)
                        {
                            HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
                        }
                        else
                        {
                            HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
                        }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case GetLoginParamsSent:
	                    if (MB0 == MB_STATUS_GOOD)
						{
					  		u32 size = LOGIN_INFO_SIZE;
							#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
								Fibre_Invalidate_Cache((u8 *) HostAdapter->MBCLoginParamBuffPointer, size);
							#endif
					  			*(HostAdapter->MBCLoginParamBuffPointer) = *((LoginInfoType *) HostAdapter->MBCommandBuffer);
							#ifdef BIG_ENDIAN
					  			BigEndConvert((u32 *) HostAdapter->MBCLoginParamBuffPointer, size/4, FC_FALSE, FC_FALSE, FC_FALSE);
							#endif
	                    	HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                	}
	                
	                	else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case GetFibreErrorsSent:

	                    if (MB0 == MB_STATUS_GOOD)
	                        {
							u8 size = ERR_LOG_TYPE_SIZE;
							*(HostAdapter->MBCUserErrLogPointer) = *((ErrorLogType *) HostAdapter->MBCommandBuffer);
							#ifdef BIG_ENDIAN
								BigEndConvert16((u16 *) HostAdapter->MBCUserErrLogPointer, size/2);
							#endif
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    }
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case DataRateSent:
	                    if (MB0 == MB_STATUS_GOOD)
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
	 
	                case SetCsFpgaOptsSent:
	                    if (MB0 == MB_STATUS_GOOD)
							HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
	                case HostSetTOVsSent:	                

	                    if (MB0 == MB_STATUS_GOOD)
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
						break;
#if 1
	                case SendDebugCmdSent:
	                    if (MB0 == MB_STATUS_GOOD)
						{
							#ifdef BIG_ENDIAN
								BigEndConvert((u32 *) HostAdapter->DbgDataBuff, HostAdapter->DbgDataLen, FC_FALSE, FC_FALSE, FC_FALSE);
							#endif
							HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
						}
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case RegDbgRetAddrSent:
	                    if (MB0 == MB_STATUS_GOOD)
							HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
#endif
	                case RegDbgTrcAddrSent:	                    if (MB0 == MB_STATUS_GOOD)
							HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case SetTestFlagsSent:
	                    if (MB0 == MB_STATUS_GOOD)
		                    if (HostAdapter->MBCCallBackFunction)
							HostAdapter->MBCCallBackFunction(0, FCRead16(HostAdapterAddress + Mailbox2));
	                    else
	                    {
		                    if (HostAdapter->MBCCallBackFunction)
	                        HostAdapter->MBCCallBackFunction(1, 0);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case SetAddOptsSent:
	                    if (MB0 == MB_STATUS_GOOD)
							HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case SetTskMngmtFlgsSent:
	                    if (MB0 == MB_STATUS_GOOD)
							HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case InitiateCSRSent:	                /* Sent Clock Sync Req to server - 06/01/01 - dfa */
	                    if (MB0 == MB_STATUS_GOOD)
	                    {
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    }
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case GetRTCSent:	                
	                    if (MB0 == MB_STATUS_GOOD)
	                    {
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    }
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case InitiateCSUSent:	                /* Sent Clock Sync Req to server - 09/19/01 - dfa */
	                    if (MB0 == MB_STATUS_GOOD)
	                    {
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    }
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case DumpRAMSent:	                /* Sent Clock Sync Req to server - 09/19/01 - dfa */
	                    if (MB0 == MB_STATUS_GOOD)
	                    {
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID);
	                    }
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID);    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

	                case DumpRAMWordSent:	                /* Sent Clock Sync Req to server - 09/19/01 - dfa */
	                    if (MB0 == MB_STATUS_GOOD)
	                    {
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID, FCRead16(HostAdapterAddress + Mailbox2));
	                    }
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID, FCRead16(HostAdapterAddress + Mailbox2));    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;
	               
	                case GetTrcAddrnSizeSent:	                /* Sent Clock Sync Req to server - 09/19/01 - dfa */
	                    if (MB0 == MB_STATUS_GOOD)
	                    {
	                        HostAdapter->MBCCallBackFunction(0, HostAdapter->MBCClientID, FCRead16(HostAdapterAddress + Mailbox2), FCRead16(HostAdapterAddress + Mailbox3));
	                    }
	                    else
	                    {
	                        HostAdapter->MBCCallBackFunction(1, HostAdapter->MBCClientID, FCRead16(HostAdapterAddress + Mailbox2), FCRead16(HostAdapterAddress + Mailbox3));    
	                    }
                        HostAdapter->MBCState = NoMBCSent;
                        break;

                    case NoMBCSent:
                        break;
                }
            }
			else
				HandleAsyncEvent(HostAdapter, MB0);
        }
        if (HostAdapter->Flags.RequestPortMap)
        {
            
            if (HostAdapter->MBCState == NoMBCSent)    
            {
                HostAdapter->Flags.RequestPortMap     = FC_FALSE; /* make sure we don't re-request it */
                HostAdapter->MBCState                 = PortMapRequestSent;
                FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_POS_MAP); /* send get port name command        */
                FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
                FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA & 0xFFFF);
                FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
                FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
                SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
            }
        }
        else if (HostAdapter->Flags.RequestLoopID)
        {
            if (HostAdapter->MBCState == NoMBCSent)     
            {
                HostAdapter->Flags.RequestLoopID      = FC_FALSE; /* make sure we don't re-request it */
                HostAdapter->MBCState                 = GetLoopIDSent;
                FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_ID); /* send get port ID       */
                SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
            }
        }
        else if (HostAdapter->Flags.RequestLoopPortLogin)
        {
            if (HostAdapter->MBCState == NoMBCSent)   
            {
                HostAdapter->Flags.RequestLoopPortLogin  = FC_FALSE; /* make sure we don't re-request it */
                HostAdapter->MBCState                    = LoopPortLoginSent;
                FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_LOGIN_LOOP_PORT); /* send Login loop port       */
                FCWrite16(HostAdapter->BaseAddress + Mailbox1, 0); /* in P2P need to login ID 0 if we are ID 1     */
                FCWrite16(HostAdapter->BaseAddress + Mailbox2, 0); 
                SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
            }
        }
        else if (HostAdapter->Flags.ReqSendDbgTrcAddr)
        {
            if (HostAdapter->MBCState == NoMBCSent)   
            {
                HostAdapter->Flags.ReqSendDbgTrcAddr  	 = FC_FALSE; /* make sure we don't re-request it */
            	if (HostAdapter->DbgDmpTrcCnt == HostAdapter->DbgDmpNumTrcWords)
				{
					HostAdapter->DbgDmpTrcBuffPtr = HostAdapter->DbgDmpTrcBuffBasePA;
					HostAdapter->DbgDmpTrcCnt = 0;
				}
				else	
					HostAdapter->DbgDmpTrcBuffPtr += HostAdapter->DbgDmpTrcSize;
				Fibre_Send_Trace_Debug_Addr(HostAdapter);            
            }
        }
        else
		{
            if (HostAdapter->MBCState == NoMBCSent)
            	MBCQueueExecuteHeadItem(HostAdapter);   /* see if any queued mailbox commands to process */
		}
    return (IntActive);
}

/* *******************************************************************************************
    HandleAsyncEvent

    Description:
        Called from service risc interrupt processing or initialization to handle async events

    Parameters:
        *HostAdapterType HostAdapter:  Pointer to the Host Adapter structure
		u16 MB0 - event
    Return Value:
        Boolean:  FC_TRUE if response is valid

    Notes:

******************************************************************************************* */
void HandleAsyncEvent(HostAdapterType *HostAdapter, u16 Event)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 MB0 = Event, MB1, MB2, MB3, MB6, MB7, i;
    switch(Event)   
    {
        case AE_LOOP_UP:
            HostAdapter->FibreStatus.LoopUp     = FC_TRUE;
            HostAdapter->FibreStatus.IDValid    = FC_FALSE;
            HostAdapter->Flags.RequestLoopID    = FC_TRUE;             /* may need to start up remote login state machine */
            HostAdapter->FibreStatus.ConnectionType = FCCT_NOT_VALID;
			if (HostAdapter->DevType >= FD_ISP_2300)
			{
                MB1 = FCRead16(HostAdapterAddress + Mailbox1);                    
                if (MB1 == 0) HostAdapter->FibreStatus.ConnectionSpeed = SPEED_1GIG;
                else HostAdapter->FibreStatus.ConnectionSpeed = SPEED_2GIG;
            }
            else
                HostAdapter->FibreStatus.ConnectionSpeed = SPEED_1GIG;
            
            if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_LOOP_UP, 0);
            FCDEBUGPRINT(("Loop UP - Async Event !\n"));
            break;

        case AE_LOOP_DOWN:
            HostAdapter->FibreStatus.LoopUp             = FC_FALSE;
            HostAdapter->Flags.RequestLoopPortLogin     = FC_FALSE;
            HostAdapter->Flags.RequestPortMap           = FC_FALSE;
            HostAdapter->Flags.RequestLoopID            = FC_FALSE;
            HostAdapter->FibreStatus.LoopTransitionCount++;
            for (i = 0; i < PORT_MAP_SIZE; i++)
                HostAdapter->FibreStatus.PortMap[i] = FC_FALSE;   /* invalidate Port Map until we get new map   */
            HostAdapter->FibreStatus.PortMapValid   = FC_FALSE;
            HostAdapter->FibreStatus.IDValid        = FC_FALSE;
            HostAdapter->FibreStatus.ConnectionType = FCCT_NOT_VALID;
            if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_LOOP_DOWN, 0);
             FCDEBUGPRINT(("Link Down - Async Event !\n"));
            break;

        case AE_SYSTEM_ERROR:
            HostAdapter->FibreStatus.LoopUp = FC_FALSE;
            FCDEBUGPRINT(("Link Down - System Error !\n"));
            MB1 = FCRead16(HostAdapterAddress + Mailbox1);
            MB2 = FCRead16(HostAdapterAddress + Mailbox2);
            MB3 = FCRead16(HostAdapterAddress + Mailbox3);
            MB6 = FCRead16(HostAdapterAddress + Mailbox6);
            FCDEBUGPRINT(("MB0=%1x MB1=%1x MB2=%1x MB3=%1x MB6=%1x\n",MB0,MB1,MB2,MB3,MB6));
            if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_SYSTEM_ERROR, MB1);

            break;

        case AE_COMMAND_COMPLETE:
            {
                TransferInfoType TI;          /* any changes here must be reflected in FCSCSI.c Handle_Resp_Entry */
                u32 TransferHandle;
                FC_Q_ERR_TYPE XferQErr;
                FCDEBUGPRINT(("SCSI Command Complete\n"));
                MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                TransferHandle = ((u32)MB2 << 16) | MB1;

                if (TransferHandle & SEND_TAG)
                    XferQErr = find_q_item(HostAdapter, FC_SEND, FC_INITIATOR, TransferHandle, &TI, FC_NULL);
                else
                    XferQErr = find_q_item(HostAdapter, FC_RECEIVE, FC_INITIATOR, TransferHandle, &TI, FC_NULL);

                TI.Status    = FTS_SUCCESS;           /* only successful transfers are fast posted    */
                TI.SCSIError = FC_NULL;

                if ((XferQErr == FC_Q_SUCCESS) && (TI.NotificationMethod != FC_NULL)) 
                    TI.NotificationMethod(&TI);       /* if we have a good record then notify app     */
            }    
            break;

        case AE_CTIO_COMPLETE:
            {
                TransferInfoType TI;          /* any changes here must be reflected in FCSCSI.c Handle_CTIO_Entry */
                u32 TransferHandle;
                FC_Q_ERR_TYPE XferQErr;
                FCDEBUGPRINT(("CTIO Complete\n"));
                MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                TransferHandle = ((u32)MB2 << 16) | MB1;


                if (TransferHandle & SEND_TAG)
                    XferQErr = find_q_item(HostAdapter, FC_SEND, FC_TARGET, TransferHandle, &TI, FC_NULL);
                else
                    XferQErr = find_q_item(HostAdapter, FC_RECEIVE, FC_TARGET, TransferHandle, &TI, FC_NULL);

                TI.Status = FTS_SUCCESS;              /* only successful transfers are fast posted    */

                if ((XferQErr == FC_Q_SUCCESS) && (TI.NotificationMethod != FC_NULL))  
                    TI.NotificationMethod(&TI);       /* if we have a good record then notify app     */
            }
            break;

        case AE_RDMA_RECEIVE_COMPLETE:
            {
                TransferInfoType TI;          /* Target RDMA's only complete with fast post   */
                
                FCDEBUGPRINT(("RDMA RX Complete\n"));
                MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get offset high */
                MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                MB3 = FCRead16(HostAdapterAddress + Mailbox3); /* get length high */
                MB6 = FCRead16(HostAdapterAddress + Mailbox6); 
                MB7 = FCRead16(HostAdapterAddress + Mailbox7); /* get Msg ID high */

                TI.TransferProtocol = FC_RDMA_TARGET;
                TI.Status     = FTS_SUCCESS;              /* only successful transfers are fast posted*/
                TI.RDMAOffset = ((u32)MB1 << 16) | (u32)MB2;
                TI.BufferSize = ((u32)MB3 << 16) | (u32)MB6;
                TI.HostID     = HostAdapter->PortID;
                TI.Port       = MB7 >> 8;
                TI.Subaddress = MB7 & 0xFF;
                TI.Direction  = FC_RECEIVE;             /* target received the data                 */
                TI.BufferAddress = 0;
                           
                if (HostAdapter->RDMANotification)        /* make sure we have a valid callback       */
                    HostAdapter->RDMANotification(&TI);   /* if we have a good record then notify app */
            }
            break;
            
        case AE_RDMA_SEND_COMPLETE:
            {
                TransferInfoType TI;          /* Target RDMA's only complete with fast post   */
                
                FCDEBUGPRINT(("RDMA TX Complete\n"));
                MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get offset high */
                MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                MB3 = FCRead16(HostAdapterAddress + Mailbox3); /* get length high */
                MB6 = FCRead16(HostAdapterAddress + Mailbox6); 
                MB7 = FCRead16(HostAdapterAddress + Mailbox7); /* get Msg ID high */

                TI.TransferProtocol = FC_RDMA_TARGET;
                TI.Status     = FTS_SUCCESS;              /* only successful transfers are fast posted*/
                TI.RDMAOffset = ((u32)MB1 << 16) | (u32)MB2;
                TI.BufferSize = ((u32)MB3 << 16) | (u32)MB6;
                TI.HostID     = HostAdapter->PortID;
                TI.Port       = MB7 >> 8;
                TI.Subaddress = MB7 & 0xFF;
                TI.Direction  = FC_SEND;                  /* target sent the data                     */
                TI.BufferAddress = 0;
                if (HostAdapter->RDMANotification)        /* make sure we have a valid callback       */
                    HostAdapter->RDMANotification(&TI);   /* if we have a good record then notify app */
            }
            break;
            
        case AE_LIP_OCCURRED:
            HostAdapter->FibreStatus.LIPCount++;
            HostAdapter->Flags.RequestPortMap = FC_TRUE;
            for (i = 0; i < PORT_MAP_SIZE; i++)
                HostAdapter->FibreStatus.PortMap[i] = FC_FALSE;   /* invalidate Port Map until we get new map   */
            HostAdapter->FibreStatus.PortMapValid = FC_FALSE;
            if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_LIP_OCCURRED, 0);
            FCDEBUGPRINT(("Lip Occurred\n"));
            break;
#ifdef IP_SUPPORT
        case AE_IP_TRANSMIT_COMPLETE:

            {
                TransferInfoType TI;          /* any changes here must be reflected in FCSCSI.c Handle_Resp_Entry */
                u32 TransferHandle;
                FC_Q_ERR_TYPE XferQErr;
                FCDEBUGPRINT(("IP Command Complete\n"));
                MB1 = FCRead16(HostAdapterAddress + Mailbox1);
                MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                TransferHandle = ((u32)MB2 << 16) | MB1;

                if (TransferHandle & SEND_TAG)
                    XferQErr = find_q_item(HostAdapter, FC_SEND, FC_INITIATOR, TransferHandle, &TI, FC_NULL);
                else
                    XferQErr = find_q_item(HostAdapter, FC_RECEIVE, FC_INITIATOR, TransferHandle, &TI, FC_NULL);

                TI.Status    = FTS_SUCCESS;           /* only successful transfers are fast posted    */
                TI.SCSIError = FC_NULL;

                if ((XferQErr == FC_Q_SUCCESS) && (TI.NotificationMethod != FC_NULL)) 
                    TI.NotificationMethod(&TI);       /* if we have a good record then notify app     */
            }    
            break;


        case AE_IP_RECEIVE_COMPLETE:
        case AE_IP_BROADCAST_RECEIVE:
        {
            TransferInfoType TI;      
            u16 RcvHandles[MAX_IP_FP_HANDLES];
            u8 i;

            FCDEBUGPRINT(("IP FAST POST RECEIVE\n"));
            TI.HostID           = HostAdapter->PortID;
            TI.TransferID       = 0;                            /* no transfer ID setup for IP receives             */
            if (MB0 == AE_IP_RECEIVE_COMPLETE)
               TI.TransferProtocol = FC_IP_TARGET;
            else
                TI.TransferProtocol = FC_IP_TARGET_BROADCAST;
            TI.Direction        = FC_RECEIVE;
            TI.Status           = FTS_SUCCESS;
            TI.SCSIError        = FC_NULL;
            TI.Subaddress       = 0;
            TI.Port             = FCRead16(HostAdapterAddress + Mailbox1) >> 8;
            TI.DescriptorPtr    = (u32 *) RcvHandles;          /* return pointer to buffer handles    */
            TI.TransferID       = ((((u32)FCRead16(HostAdapterAddress + Mailbox1)) << 24) |
                                  FCRead16(HostAdapterAddress + Mailbox2)) & 0x00FFFFFFL;
            TI.BufferSize       = FCRead16(HostAdapterAddress + Mailbox3);
            TI.DescriptorCount  = FCRead16(HostAdapterAddress + Mailbox6); /* number of handles used                */
            for (i = 0; i < TI.DescriptorCount; i++)
                RcvHandles[i] = FCRead16(HostAdapterAddress + Mailbox10 + (i<<1));
            if (HostAdapter->TargetHandler != FC_NULL)          /* if we have a good record then notify app         */
                HostAdapter->TargetHandler(&TI);
            break;
        }
            
           
#endif /* ifdef IP_SUPPORT */

        case AE_CONNECTED_P2P:
             if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_CONNECTED_POINT_TO_POINT, 0);
             break;
                          
        case AE_WATCHDOG_TIMEOUT:
             if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_WATCHDOG_TIMEOUT, 0);
             break;               

        case AE_CHANGE_NOTIFICATION:
             if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_FABRIC_CHANGE, 0);
             break;
             
        case AE_DATABASE_CHANGED:
             if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_PORT_DB_CHANGE_OCCURRED, 0);
             break;
             
        case AE_NO_IP_RCV_BUFFERS:
             if (HostAdapter->StatusHandler)
                HostAdapter->StatusHandler(HostAdapter->PortID, FCS_OUT_OF_IP_RCV_BUFFERS, 0);
             break;
                   
                case AE_PLOGI_RCVD:
                {
                     u32 S_ID;
                     u32 IDs;
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get loopid | S_ID-h (MSB) */
                     MB2 = FCRead16(HostAdapterAddress + Mailbox2); /* get S_ID_l */
                     S_ID = (MB1 << 16);
                     IDs = S_ID | MB2;						/* loopid in MSB, followed by SID*/
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_PLOGI_OCCURRED, IDs);
					 FCDEBUGPRINT(("PLOGI Occurred\n"));
                     break;
				}
                case AE_PRLI_RCVD:
                {
                     u32 S_ID;
                     u32 IDs;
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get loopid | S_ID-h (MSB) */
                     MB2 = FCRead16(HostAdapterAddress + Mailbox2); /* get S_ID_l */
                     S_ID = (MB1 << 16);
                     IDs = S_ID | MB2;						/* loopid in MSB, followed by SID*/
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_PRLI_OCCURRED, IDs);
					 FCDEBUGPRINT(("PRLI Received\n"));
                     break;
				}
                case AE_PRLI_ACC_RCVD:
                {
                     u32 S_ID;
                     u32 IDs;
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get loopid | S_ID-h (MSB) */
                     MB2 = FCRead16(HostAdapterAddress + Mailbox2); /* get S_ID_l */
                     S_ID = (MB1 << 16);
                     IDs = S_ID | MB2;						/* loopid in MSB, followed by SID*/
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_PRLI_ACC_OCCURRED, IDs);
					 FCDEBUGPRINT(("PRLI ACC Received\n"));
                     break;
				}
                case AE_PDISC_RCVD:
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); 
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_PDISC_OCCURRED, MB1);
					 FCDEBUGPRINT(("PDISC Received\n"));
                     break;

                case AE_PDISC_ACC_RCVD:
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); 
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_PDISC_ACC_OCCURRED, MB1);
					 FCDEBUGPRINT(("PDISC ACC Received\n"));
                     break;

                case AE_READY:
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); 
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_READY, MB1);
					 FCDEBUGPRINT(("Ready Received\n"));
                     HostAdapter->FibreStatus.SyncValid = FC_TRUE;
                     break;

                case AE_LOS:
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_LOS, 0);
					 FCDEBUGPRINT(("LOS occurred\n"));
                     HostAdapter->FibreStatus.SyncValid = FC_FALSE;
                     break;

				case AE_CSU_RCVD:
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_CSU_OCCURRED, 0);
					FCDEBUGPRINT(("CSU Occurred\n"));
                    break;

                case AE_CSR_RCVD:
                {
                    #define MAX_CSU_DATA_RECS 31    /* maximum 31 mailboxes to read
                                                    // should only be 12 with data
                                                    // MB2-MB13 CSU data
                                                    // MB0 - AE status
                                                    // MB1 - # of MBs to read*/
                    TransferInfoType TI;      
                    u16 RcvCSRData[MAX_CSU_DATA_RECS];
                    u8 i,count;
                    
                    /* read count of CSHW data (2bytes per MBX) to host
                    // if count is 0, then AE was queued, and there is not
                    // enough MB AE queue space in FW to send all data*/
                    if ((count = FCRead16(HostAdapterAddress + Mailbox1)))
                    {
                        /* check limit - max 31 mailboxes left to read*/
                        if (count > MAX_CSU_DATA_RECS) 
                            count = MAX_CSU_DATA_RECS;
                        /* read MB2-MB13 */   
                        for (i = 0; i < count; i++)
                            RcvCSRData[i] = FCRead16(HostAdapterAddress + Mailbox2 + (i<<1));

                        TI.DescriptorPtr    = (u32 *) RcvCSRData;
                        TI.Status           = FTS_CSR_RCVD;
                    
                        /* Notify user via registered user handler*/
                        if (HostAdapter->TargetHandler != FC_NULL)
                            HostAdapter->TargetHandler(&TI);
                    }
                    else
                        FCDEBUGPRINT(("CSR Occurred, but AE FW queue full\n"));
                    
                    FCDEBUGPRINT(("CSR Occurred\n"));
                    
					break;
                 }
                 
                case AE_LIP_ERROR:
                    MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get error status code (LP or AL TO) */
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_LIP_ERROR_OCCURRED, MB1);
					FCDEBUGPRINT(("Lip Error Occurred\n"));
                    break;
                
                case AE_RCV_Q_FULL:
					FCDEBUGPRINT(("Target Queue full\n"));
                    break;

/* Used for testing only  */
                case AE_XFER_VIOL:
                    MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get error status code (LP or AL TO) */
                    MB2 = FCRead16(HostAdapterAddress + Mailbox2); /* get error status code (LP or AL TO) */
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_XFER_ERROR, (MB1<<16)|MB2);
					break;

                case AE_SEQ_INT:
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get S_ID_l */
                    FCDEBUGPRINT(("Seq int = %X\n",MB1));
					break;

                case AE_SEQ_RET:
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get S_ID_l */
                    FCDEBUGPRINT(("Seq ret xcb = %X\n",MB1));
					break;

                case AE_ACK_SENT:
                    FCDEBUGPRINT(("ACK sent"));
					break;

                case BP_ACTIVE:
                    if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_BP_ACTIVE, FCRead16(HostAdapterAddress + Mailbox1));
                    FCDEBUGPRINT(("BP active"));
					break;

                case AE_DMP_TRC_RCVD:
                     MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get S_ID_l */
                     MB2 = FCRead16(HostAdapterAddress + Mailbox2); /* get S_ID_l */
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_DBG_DMP_RCVD, (MB2<<16)|(MB1&0xffff));
					break;

                case AE_DMP_TRC_START:
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_DBG_TRC_STRT, 0);
					break;

                case AE_DMP_TRC_STOP:
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_DBG_TRC_STP, 0);
					break;

                case AE_R_T_TOV:
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_R_T_TOV, 0);
					 break;

                case AE_AL_TIME:
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_AL_TIME, 0);
					 break;

                case AE_LP_TOV:
                     if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_LP_TOV, 0);
                     break;
					
                default:
                   FCDEBUGPRINT(("Unhandled Event %4X\n", MB0));
                        MB1 = FCRead16(HostAdapterAddress + Mailbox1); /* get offset high */
                        MB2 = FCRead16(HostAdapterAddress + Mailbox2);
                        MB3 = FCRead16(HostAdapterAddress + Mailbox3); /* get length high */
                   if (HostAdapter->StatusHandler)
                        HostAdapter->StatusHandler(HostAdapter->PortID, FCS_UNHANDLED_EVENT, MB0);
                   break;
    }
#ifndef ISP24XX
	if (HostAdapter->DevType < FD_ISP_2300)
        /* clear semaphore */
        FCWrite16( HostAdapterAddress + IspSemaphore, SEMAPHORE_UNLOCK);
#endif
}
/* *******************************************************************************************
    ProcessPortMapResponse

    Description:
        Called from risc interrupt processing to handle the repsonse to a port posistion map
        mailbox command

    Parameters:
        *HostAdapterType HostAdapter:  Pointer to the Host Adapter structure

    Return Value:
        Boolean:  FC_TRUE if response is valid

    Notes:

******************************************************************************************* */
Boolean ProcessPortMapResponse(HostAdapterType *HostAdapter)
{
    u8 *MapBuffer = HostAdapter->MBCommandBuffer;  /* map is returned in MailboxCommandBuffer */
    u16 i;
    u16 PortCount;


#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
    Fibre_Invalidate_Cache(MapBuffer, PORT_MAP_SIZE);
#endif

    if (HostAdapter->MB0 != MB_STATUS_GOOD)
        return FC_FALSE;
    else
#ifndef FIXED_32BIT
    {
        PortCount = *MapBuffer++;                       /* first entry is number of ports on the loop   */
        for (i = 0; i < PortCount; i++)
            HostAdapter->FibreStatus.PortMap[AL_PA_TO_ID[*MapBuffer++]] = FC_TRUE;
        HostAdapter->FibreStatus.PortMap[0x7F] = FC_FALSE; /* in case of any bogus AL_PA's              */
    }
#else
    {
        PortCount = (u32)EXTRACT_BYTE(MapBuffer,0);     /* first entry is number of ports on the loop   */
        for (i = 0; i < PortCount; i++)
            HostAdapter->FibreStatus.PortMap[AL_PA_TO_ID[EXTRACT_BYTE(MapBuffer,i+1)]] = FC_TRUE;
        HostAdapter->FibreStatus.PortMap[0x7F] = FC_FALSE; /* in case of any bogus AL_PA's              */
    }
#endif
    return FC_TRUE;
}

/* *******************************************************************************************
    SendMarker

    Description:
        Called from risc interrupt processing to handle the repsonse to reset and abort
        mailbox commands

    Parameters:
        *HostAdapterType HostAdapter:  Pointer to the Host Adapter structure
        u16 Modifier: specifies whether to clear LUN, Target or all 

    Return Value:
        None

    Notes:

******************************************************************************************* */
void SendMarker(HostAdapterType *HostAdapter, u16 Modifier)
{

    u32 *Temp_Ptr;
    tMarkerIOCB  *ReqQ_Ptr;
    u16 i;

    /* put marker on queue so FC2100 knows to stop aborting any new IOCB's placed on the Queue */
    ReqQ_Ptr =(tMarkerIOCB  *)NextReqQEntryAddress(HostAdapter);
    Temp_Ptr = (u32 *)ReqQ_Ptr;
    for(i=0;i<REQ_RESP_QUEUE_ENTRY_SIZE;i=i+4)     /* zero the entire Q entry */
        *Temp_Ptr++ = 0;
    ReqQ_Ptr->Header.EntryType =MARKER_TYPE ; /* MARKER entry type */
    ReqQ_Ptr->Header.EntryCount= 0x01;        /* entry count (always 1) */
    ReqQ_Ptr->Header.SequenceNum=0;
    ReqQ_Ptr->LUN = HostAdapter->ResetingLUN;
    ReqQ_Ptr->LoopID = HostAdapter->AbortingTargetID;
    ReqQ_Ptr->Modifier = Modifier;       

#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT 
    Fibre_Flush_Cache((u8*) ReqQ_Ptr, sizeof(*ReqQ_Ptr));
#endif
#ifdef BIG_ENDIAN
    BigEndConvert((u32 *)ReqQ_Ptr, STATUSTYPECWORDS, STATUSTYPESENSE, STATUSTYPECDB, FC_TRUE);
#endif
    Incr_ReqQ_Inindex(HostAdapter);/*//*/
}

/************************************************************************
*
*  Incr_RespQ_Outindex
*
*************************************************************************
*
* This function updates the Response Queue Out pointer
*
* Input : HostAdapter
*
* Return: none
*
************************************************************************/
void Incr_RespQ_Outindex(HostAdapterType *HostAdapter)
{
/* //^^^^^ add check for queue empty */
    HostAdapter->RespQTailIndex++;
    HostAdapter->RespQTailIndex %= REQ_RESP_QUEUE_NUM_ENTRIES;
    /* update resp Q in pointer mbox 5 */
if (HostAdapter->DevType < FD_ISP_2300)
    FCWrite16(HostAdapter->BaseAddress + Mailbox5, HostAdapter->RespQTailIndex);
else
    FCWrite16(HostAdapter->BaseAddress + IspRespQOutPtr, HostAdapter->RespQTailIndex);
}


/*************************************************************************
*
*  Incr_ReqQ_Inindex
*
*************************************************************************
*
* This function updates the Request Queue In pointer
*
* Input : HostAdapter
*
* Return: none
*
************************************************************************/
void Incr_ReqQ_Inindex(HostAdapterType *HostAdapter)
{
/* ^^^^^ add check for queue Full */
    HostAdapter->ReqQHeadIndex++;
    HostAdapter->ReqQHeadIndex %= REQ_RESP_QUEUE_NUM_ENTRIES;
    /* update req Q in pointer mbox 4 */
if (HostAdapter->DevType < FD_ISP_2300)
    FCWrite16(HostAdapter->BaseAddress + Mailbox4, HostAdapter->ReqQHeadIndex);
else
    FCWrite16(HostAdapter->BaseAddress + IspReqInPtr, HostAdapter->ReqQHeadIndex);

}


/*************************************************************************
*
*  Incr_ReqQ_Inindex_with_Cont
*
*************************************************************************
*
* This function updates the Request Queue In pointer
*
* Input : HostAdapter, NumEntries
*
* Return: none
*
************************************************************************/
void Incr_ReqQ_Inindex_with_Cont(HostAdapterType *HostAdapter, u16 NumEntries)
{
/* ^^^^^ add check for queue Full */
    HostAdapter->ReqQHeadIndex+= NumEntries;
    HostAdapter->ReqQHeadIndex %= REQ_RESP_QUEUE_NUM_ENTRIES;
    /* update req Q in pointer mbox 4 */
if (HostAdapter->DevType < FD_ISP_2300)
    FCWrite16(HostAdapter->BaseAddress + Mailbox4, HostAdapter->ReqQHeadIndex);
else
    FCWrite16(HostAdapter->BaseAddress + IspReqInPtr, HostAdapter->ReqQHeadIndex);
}


#ifdef BIG_ENDIAN
/************************************************************************************************
 *
 * Function byteswap
 *
 *  Input:   value - an unsigned 16 bit integer to convert between Big Endian and Little Endian.
 *
 *  Return: u16 - the converted number.
 *
 * To perform the conversion between Big and Little Endian for a unsigned 16 bit number is
 * simply a matter of a byte swap.
 *
 ************************************************************************************************/
u16 byte_swap16(u16 value)
{
    return( ((value >> 8)&0xFF) | ((value<<8)&0xFF00)  );
}

/************************************************************************************************
 *
 * Function wordswap
 *
 *  Input:   value - an unsigned 32 bit integer to convert between Big Endian and Little Endian.
 *
 *  Return: u32 - the converted number.
 *
 * To perform the conversion between Big and Little Endian for a unsigned 32 bit number with 16 bit
 * boundaried requires Big Endian conversion followed by a word swap.
 *
 ************************************************************************************************/
u32 word_swap32(u32 value)
{
    return( ((value >> 16)&0xFFFF) | ((value<<16)&0xFFFF0000)  );
}

/************************************************************************************************
 *
 * Function BigEndConvert
 *
 *  Input:  base - base address of the message to convert.
 *          words2convert - the number of longwords in the message.
 *          HasSenseField - indicates the message has a sense field.
 *          HasCDBArray - indicates the message has a CDB array.
 *          HasHeader - has an IOCB Header for the first 32-bit word.
 *
 * Note: this function will only work with the QLogic firmware data structures, this function
 *       is NOT a generic little endian to big endian convertor.
 *
 * The q_logic firmware accepts messages that have been defined in a little endian format.
 * If the host processor uses a big endian format, this routine in conjunction with the
 * conditionally compiled data structures will reformat the command structures to a little
 * endian format.
 *
 * If the conditional compile variable BIG_ENDIAN is not defined, this function will return
 * immediately.
 *
 * The conversion is performed in two phases
 *   - the data structure definition
 *   - the routine BigEndConvert
 *
 * The data structures have been defined to exploit the BigEndConvert function.  Each longword
 * of each message type was defined in byte reverse order for the BIG_ENDIAN conditional compile.
 * Exceptions to this rule were reserved words and character arrays appearing in the middle of a
 * structure definition.  Character arrays appearing in the middle of a structure posed a special
 * problem that could be characterized as one of two cases (handled by the two booleans employed
 * by this routine).
 *
 * The routine BigEndConvert performs byte swapping on the firmware message.  The parameter
 * LWords2Convert identifies the last longword in the message to convert.  The first longword
 * in most messages (the IOCB Header) does not need to be converted.  The IOCB Header has the
 * same format for both little endian and big endian formats.  The longword byte conversion
 * is as follows
 *      output_byte_1 = input_byte_4
 *      output_byte_2 = input_byte_3
 *      output_byte_3 = input_byte_2
 *      output_byte_4 = input_byte_1
 * The header files defining the message data structures (FCIocb.h & FCMbcmds.h) also provides
 * 3 defines
 *      - the number of longwords to convert
 *      - an indicator if the message has a Sense Field
 *      - an indicator if the message has a CDB field
 * The two indicators handle two special cases where character arrays appear in the middle
 * of the structure definitions.  Character arrays do not need any special conversion between
 * little endian and big endian.  If the flag HasCDBArray is set, the message has a CDB character
 * array (consistantly appearing at longwords 5->8).  If the flag HasSenseField is set, the
 * message structure has a 18 element character array starting in the lower 16-bit word of the
 * 9_th longword.  The Sense array is always predicated by a 16-bit field SCSI_Status which
 * must be byte swapped.
 * A third flag, HasHeader is also defined.  If set to FC_TRUE, the message has a standard
 * (IOCB) header (that should not be converted).  If set to FC_FALSE, the message does not include
 * the standard header and thus the first word should be converted.
 *
 *
 ************************************************************************************************/

void BigEndConvert (u32 base[],
                    u32 LWords2Convert,
                    Boolean HasSenseField,
                    Boolean HasCDBArray,
                    Boolean HasHeader)
{
u32 reference;
u32 reversed;
u32 i;
u8 *char_ptr;


   /* Convert longwords 1 (or 0) through LWords2Convert - 1.  */
   /* If the message stucture has a CDB array, skip  over     */
   /* the array block.  HasHeader dictates the first word     */
   /* to be converted.                                        */

   if (FC_TRUE == HasHeader)
        i = 1;
    else
        i = 0;

    for (; i < LWords2Convert; i++)
    {
        if (HasCDBArray && (CBD_START_ELEMENT == i))
            i += CBD_BLOCK_SIZE;

        reference = base[i];

        reversed = (((reference >> 24) & 0xFF)      ) |
                   (((reference >> 16) & 0xFF) << 8 ) |
                   (((reference >> 8)  & 0xFF) << 16) |
                   (((reference)       & 0xFF) << 24);

        base[i] = reversed;
    }

    /* If the array has a Sense array, perform a byte swap  */
    /* on the upper 16 bits of the 9_th (SENSE_FIELD_LWORD) */
    /* longword.                                            */

    if (HasSenseField)
    {
        char_ptr = (u8 *)&base[SENSE_FIELD_LWORD];
        i = char_ptr[0];
        char_ptr[0] = char_ptr[1];
        char_ptr[1] = i;
    }
}

void BigEndConvert16 (u16 base[], u32 Words2Convert)
{
u16 reference;
u16 reversed;
u32 i = 0;

   /* Convert word 0 through Words2Convert - 1.           */

    for (; i < Words2Convert; i++)
    {
        reference = base[i];

        reversed = (((reference >> 8)  & 0xFF)      ) |       /* B1 */
                   (((reference)       & 0xFF) << 8 ) ;       /* B0 */

        base[i] = reversed;
    }
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT
	Fibre_Flush_Cache((u8 *)base, Words2Convert * sizeof(u16));
#endif
}
#endif

#if defined CHECK_SYNCHRONIZATION
void CheckSynchronization(HostAdapterType *HostAdapter)
{
    if (HostAdapter->EntryCounter)
        HostAdapter->SyncErr = FC_TRUE; /* Very Bad if this happens */

    HostAdapter->EntryCounter++;
}

void ClearCheckSynchronization(HostAdapterType *HostAdapter)
{
    HostAdapter->EntryCounter--;
}

#endif



/* Mailbox Command Queue insert item */

MBCQueueErrorType MBCQueueInsertItem(HostAdapterType *HostAdapter, MBCQueueItemType *MBCQueueItem)
{

    if (HostAdapter->MBCQueueNumEntries == MailboxCommandQueueSize)
        return (MBCQueueFull);
    else
    {
         HostAdapter->MBCQueueBasePtr[HostAdapter->MBCQueueHead++] = *MBCQueueItem;    /* copy record          */
         if (HostAdapter->MBCQueueHead == MailboxCommandQueueSize)                     /* account for wrap     */ 
             HostAdapter->MBCQueueHead = 0;
         HostAdapter->MBCQueueNumEntries++;
    }
    return (MBCQueueSuccess);
}


/* Mailbox Command Queue Execute Item */  
MBCQueueErrorType MBCQueueExecuteHeadItem(HostAdapterType *HostAdapter)
{
    MBCQueueItemType *MBCQItemPtr; 
    if (HostAdapter->MBCQueueNumEntries == 0)
        return (MBCQueueEmpty);
    else
    {
         MBCQItemPtr = &HostAdapter->MBCQueueBasePtr[HostAdapter->MBCQueueTail++];
         if (HostAdapter->MBCQueueTail == MailboxCommandQueueSize)                     /* account for wrap     */ 
             HostAdapter->MBCQueueTail = 0;
         HostAdapter->MBCQueueNumEntries--;

        HostAdapter->MBCState             = MBCQItemPtr->MBCState;
        HostAdapter->MBCCallBackFunction  = MBCQItemPtr->Callback;
        HostAdapter->MBCClientID          = MBCQItemPtr->ClientID;
        HostAdapter->AbortingTargetID     = MBCQItemPtr->AbortingTargetID;
        HostAdapter->ResetingLUN          = MBCQItemPtr->ResetingLUN;
        HostAdapter->MBCUserLESBPointer   = MBCQItemPtr->MBCUserLESBPointer;  
        HostAdapter->MBSNameIdListPointer = MBCQItemPtr->MBSNameIdListPointer;
        HostAdapter->WWNPtr               = MBCQItemPtr->WWNPtr;
        HostAdapter->FabricTargetPortLoopIDAlias = MBCQItemPtr->MBCAlias;
        /* handle special case commands */
        if (MBCQItemPtr->MBCState == DiagnosticEchoSent || MBCQItemPtr->MBCState == LoopBackTestSent)
        {
            FCWrite16(HostAdapter->BaseAddress + Mailbox10, MBCQItemPtr->Mailbox[10]); 
            FCWrite16(HostAdapter->BaseAddress + Mailbox11, MBCQItemPtr->Mailbox[11]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox12, MBCQItemPtr->Mailbox[12]);                 
            FCWrite16(HostAdapter->BaseAddress + Mailbox13, MBCQItemPtr->Mailbox[13]);                            FCWrite16(HostAdapter->BaseAddress + Mailbox14, MBCQItemPtr->Mailbox[14]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox15, MBCQItemPtr->Mailbox[15]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox16, MBCQItemPtr->Mailbox[16]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox17, MBCQItemPtr->Mailbox[17]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox18, MBCQItemPtr->Mailbox[18]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox19, MBCQItemPtr->Mailbox[19]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox20, MBCQItemPtr->Mailbox[20]);
            FCWrite16(HostAdapter->BaseAddress + Mailbox21, MBCQItemPtr->Mailbox[21]);
        }
        switch (MBCQItemPtr->NumMailboxRegs)  /* we want all lower MBs filled (except 4 and 5 */
        {
            case 6:
                FCWrite16(HostAdapter->BaseAddress + Mailbox7, MBCQItemPtr->Mailbox[7]);
            case 5:
                FCWrite16(HostAdapter->BaseAddress + Mailbox6, MBCQItemPtr->Mailbox[6]);
            case 4:
                FCWrite16(HostAdapter->BaseAddress + Mailbox3, MBCQItemPtr->Mailbox[3]);
            case 3:
                FCWrite16(HostAdapter->BaseAddress + Mailbox2, MBCQItemPtr->Mailbox[2]);
            case 2:
                FCWrite16(HostAdapter->BaseAddress + Mailbox1, MBCQItemPtr->Mailbox[1]);
            case 1:
                FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBCQItemPtr->Mailbox[0]);
        }        
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);  /* tell Qlogic to execute the command */

    }
    return (MBCQueueSuccess);
}

KeyType Fibre_Read_Key(HostAdapterType *HostAdapter)
{
	KeyType Key;
    u16 Temp, Addr;
    u8 i;

    if ((FCRead16(HostAdapter->BaseAddress + IspCtlSts) & 0xC000) == 0x4000) /* check if port 2 of 2312 */
		Addr = EEPROM_KEY_START/2 + 0x80;
	else
		Addr = EEPROM_KEY_START/2;
	for (i = 0; i < sizeof(Key)/2; i++)
    {
        Temp = ReadEEPROM(i+Addr,HostAdapter->BaseAddress);
        Key.Bytes[i*2] = Temp & 0xFF;
        Key.Bytes[(i*2) + 1] = (Temp >> 8) & 0xFF;
    }
    return Key;
}

Boolean Send_Soft_Key(HostAdapterType *HostAdapter, KeyType Key)
{
    BaseAddressType HostAdapterAddress = HostAdapter->BaseAddress;
    u16 Status;
    if (!WaitForMboxReady(HostAdapterAddress))
	{
#if 0
{
	u8 i;
	for (i=0; i<8; i++)
		printf("B%d=%0X ", i, Key.Bytes[i]);
	printf("\n");
}
#endif	   
	    FCWrite16(HostAdapterAddress + Mailbox0, MBC_SEND_SW_KEY);
	    FCWrite16(HostAdapterAddress + Mailbox1, (Key.Bytes[1]<<8)|(Key.Bytes[0]));
	    FCWrite16(HostAdapterAddress + Mailbox2, (Key.Bytes[3]<<8)|(Key.Bytes[2]));
	    FCWrite16(HostAdapterAddress + Mailbox3, (Key.Bytes[5]<<8)|(Key.Bytes[4]));
	    FCWrite16(HostAdapterAddress + Mailbox6, (Key.Bytes[7]<<8)|(Key.Bytes[6]));

        SET_HOST2RISC_INTR(HostAdapterAddress);
	{
		volatile u32 Counter = 10;
		while (Counter>0)
		{
			Status = WaitForMbCmdCmpltServiceAEs(HostAdapter);
			if (Status == 0)	Counter--;
			else break;
		}
	}
		if (Status)
		{
	    	FCDEBUGPRINT(("Sent Key, status = %X\n", Status));
			return FC_TRUE;
		}
		else
		{
			FCDEBUGPRINT(("Send softkey failed - Mailbox not return!!!"));
			return FC_FALSE;
		}
	}
	else
	{
		FCDEBUGPRINT(("Cannot send softkey - Mailbox not ready!!!"));
		return FC_FALSE;
	}
}
