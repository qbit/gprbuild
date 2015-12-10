/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             eeprom.h    $Revision: 1.1 $
    Module Description:     routines to read data from FC2100 EEPROM


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
#ifndef EEPROM_H
#define EEPROM_H

#ifndef ISP24XX
#define EEPROM_SIZE 128  /* 128 X 16 */

#define ADDRESS_LENGTH  8                 /* 8th bit not used in 93c56 */
#define COMMAND_LENGTH  (ADDRESS_LENGTH + 3)
#define DATA_LENGTH     16

#define EEPROM_READ     (0x18 << (ADDRESS_LENGTH - 2))
#define EEPROM_EWEN     (0x13 << (ADDRESS_LENGTH - 2))
#define EEPROM_ERASE    (0x1C << (ADDRESS_LENGTH - 2))
#define EEPROM_WRITE    (0x14 << (ADDRESS_LENGTH - 2))
#define EEPROM_ERAL     (0x12 << (ADDRESS_LENGTH - 2))
#define EEPROM_WRAL     (0x11 << (ADDRESS_LENGTH - 2))
#define EEPROM_EWDS     (0x10 << (ADDRESS_LENGTH - 2))


/* set/clear Data In to to EEPROM */
#define SET_EEPROM_DI(HostAdapterBaseAddress)   (FCWrite16(HostAdapterBaseAddress + IspNVRAM,\
                         FCRead16(HostAdapterBaseAddress + IspNVRAM) | 4))
#define CLR_EEPROM_DI(HostAdapterBaseAddress)   (FCWrite16(HostAdapterBaseAddress + IspNVRAM,\
                         FCRead16(HostAdapterBaseAddress + IspNVRAM) & ~4))
/* Read EEPROM Data out */
#define EEPROM_DO(HostAdapterBaseAddress) ((FCRead16(HostAdapterBaseAddress + IspNVRAM) & 8) >> 3)

/* set/clear chip select to to EEPROM */
#define SET_EEPROM_CS(HostAdapterBaseAddress)   (FCWrite16(HostAdapterBaseAddress + IspNVRAM,\
                         FCRead16(HostAdapterBaseAddress + IspNVRAM) | 2))
#define CLR_EEPROM_CS(HostAdapterBaseAddress)   (FCWrite16(HostAdapterBaseAddress + IspNVRAM,\
                         FCRead16(HostAdapterBaseAddress + IspNVRAM) & ~2))

/* set/clear Clock to to EEPROM */
#define SET_EEPROM_CLK(HostAdapterBaseAddress)   (FCWrite16(HostAdapterBaseAddress + IspNVRAM,\
                         FCRead16(HostAdapterBaseAddress + IspNVRAM) | 1))
#define CLR_EEPROM_CLK(HostAdapterBaseAddress)   (FCWrite16(HostAdapterBaseAddress + IspNVRAM,\
                         FCRead16(HostAdapterBaseAddress + IspNVRAM) & ~1))


static void ClockEEPROM(BaseAddressType HostAdapterBaseAddress)
{
   SET_EEPROM_CLK(HostAdapterBaseAddress);

   CLR_EEPROM_CLK(HostAdapterBaseAddress);
}

static void SendEEPROM(u16 Data, u16 Count, BaseAddressType HostAdapterBaseAddress)
{
   Data <<= (16 - Count);  /* get MSB of real data into MSB of Data */

   while (Count != 0)
   {
      if (Data & 0x8000)
         SET_EEPROM_DI(HostAdapterBaseAddress);
      else
         CLR_EEPROM_DI(HostAdapterBaseAddress);
      ClockEEPROM(HostAdapterBaseAddress);
      Count--;
      Data <<=1;
   }
   CLR_EEPROM_DI(HostAdapterBaseAddress);  /* keep low when done */
}

static u16 ReadEEPROM(u16 Address, BaseAddressType HostAdapterBaseAddress)
{
    u16 Command = EEPROM_READ;
    int i;
    u16 Data = 0;

    Command |= Address;
    SET_EEPROM_CS(HostAdapterBaseAddress);
    SendEEPROM(Command, COMMAND_LENGTH, HostAdapterBaseAddress);
    for (i =0; i < DATA_LENGTH; i++)
    {
       ClockEEPROM(HostAdapterBaseAddress);
       Data = (Data << 1) | EEPROM_DO(HostAdapterBaseAddress);
    }
    CLR_EEPROM_CS(HostAdapterBaseAddress);
    return Data;
}

#else  /* ISP24XX */

static u16 ReadEEPROM(u16 Address, BaseAddressType HostAdapterBaseAddress)
{
    int i;
    u16 Data = 0;

    

    return Data;
}

#endif /* ifndef ISP24XX */

#endif /* ifndef EEPROM_H */
