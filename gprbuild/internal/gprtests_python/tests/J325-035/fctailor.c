/* ********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FCTAILOR.c   $Revision: 1.2 $
    Module Description:     Host specific definitions for the FC-2100 API

  Special Notes:
        
  
  Revision History
    Date        Description of Change
    ---------   ------------------------
    6/9/99      Version 2.4 See rel2_4.txt
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


void	INTERRUPT_MemoryBarrier(void);


const Boolean InterruptMode = FC_TRUE;   /* Set this to FC_TRUE only when using              */
                                         /* interrupts to call Fiber_Service                 */
                                         /* this define enables the Host adapter interrupts  */

const u8 MaxConcurrentCommands = 128;    /* set to the max number of active commands at a time*/

const u8 MailboxCommandQueueSize = 20;  /* sets the number of mailbox commands that can be queued */

#ifdef IP_SUPPORT
const u16 NumIPRcvBufQEntries = 64;     /* This sets the number of receive buffers that can    */
                                         /* be held in the container Queue                      */                                          
#endif                                   /* ***** THIS IS NOT USED FOR FAST IP BUFFERING *****  */


/**** Fast processor compensation  ****/
const u32 FC2100ResetDelay = 50000;     /* Reset delay - may need to be increased for faster */
                                        /* processors                                        */
                                        /* symptom is exception or hang after soft HW reset  */
                                        /* in Fibre_Self_Test and Fibre_Initialization       */
                                        /* If you can single step and it works,              */
                                        /* double this value.                                */

/* The user provides these memory allocation routines specific to their system      */
/* this example use standard C malloc and free                                      */
#include <stdlib.h>
void *Fibre_Malloc(u32 SIZE)                       
{
    return (malloc(SIZE));
}

void Fibre_Free(void *PTR, u32 SIZE)      /* SIZE added for Solaris support which requires size on free */                    
{
    free(PTR);
}


/*************************************************************************
*
*  FC_Physical_To_Processor
*
*************************************************************************
*
* This function performs a physical address to a processor address translation
*
* Input : u32 PhysicalAddress
*
* Return: void *ProcessorAddress
*
************************************************************************/

void* FC_Physical_To_Processor(u32 PhysicalAddress)
{
    return((void *)PhysicalAddress); /* One to one mapping in extended DOS demo application */
}


/*************************************************************************
*
*  FC_Processor_To_Physical
*
*************************************************************************
*
* This function performs a processor to physical (PCI) address translation
*
* Input : void *ProcessorAddress
*
* Return: u32 PhysicalAddress
*
************************************************************************/

u32 FC_Processor_To_Physical(void *ProcessorAddress)
{
    return((u32) ProcessorAddress); /* One to one mapping in extended DOS demo application */
}

/* Modify these adapter read and write functions as required by the specific host system.   */
/* Access to the FC-2100 host adapters can either be memory mapped or I/O mapped.           */
/* Choose the method best suited to the host system.                                        */
/* Note that the proper base address must be specifed to the Fibre_Initialize function      */
/* depending on whether I/O or memory mapped accesses are selected.                         */


#if 1
/************************************************************************
 *                        Memory mapped access
 ************************************************************************/
void FCWrite16(BaseAddressType ADDRESS, u16 VALUE)
{
#if !defined BIG_ENDIAN
    *((u16 *)(ADDRESS)) = VALUE;         /* memory write                                     */
#else
    *((u16 *)(ADDRESS)) = (((VALUE)>>8)&0xFF) | (((VALUE)<<8)&0xFF00);
#endif
}

#ifdef ISP24XX
void FCWrite32(BaseAddressType ADDRESS, u32 VALUE)
{
#if !defined BIG_ENDIAN
    *((u32 *)(ADDRESS)) = VALUE;         /* memory write                                     */
#else
    *((u32 *)(ADDRESS)) = (((VALUE)>>24)&0xFF) | (((VALUE)>> 8)&0xFF00) | (((VALUE) << 24)&0xFF000000) | (((VALUE)<< 8)&0xFF0000);
#endif
}
#endif


u16 FCRead16(BaseAddressType ADDRESS)
{
#if !defined BIG_ENDIAN
    return (*((u16 *)(ADDRESS)));         /* memory read                                     */
#else
    return (byte_swap16(*((u16 *)(ADDRESS))));
#endif   
}

u32 FCRead32(BaseAddressType ADDRESS)
{
#if !defined BIG_ENDIAN
    return (*((u32 *)(ADDRESS)));         /* 32 bit memory read                               */
#else
    u32 reference;
    reference = *((u32 *)(ADDRESS));  
    return ((((reference >> 24) & 0xFF)) |
             (((reference >> 16) & 0xFF) << 8 ) |
             (((reference >> 8)  & 0xFF) << 16) |
             (((reference)       & 0xFF) << 24));
#endif
}
#else
/* Example for I/O mapped access */
#include <conio.h>
void FCWrite16(BaseAddressType ADDRESS, u16 VALUE)
{
    outpw(ADDRESS, VALUE);
}
#ifdef ISP24XX
void FCWrite32(BaseAddressType ADDRESS, u32 VALUE)
{
    outpd(ADDRESS, VALUE);
}
#endif
u16 FCRead16(BaseAddressType ADDRESS)
{
    return inpw(ADDRESS);  
}

u32 FCRead32(BaseAddressType ADDRESS)
{
    return inpd(ADDRESS);
}
#endif




/* Interrupt and mutual exlusion functions - see users guide */


extern Boolean FibreLockAda    (u8 HostID);
extern Boolean FibreReleaseAda (u8 HostID);


Boolean Fibre_Lock(u8 HostID)
{
    /*  Call Ada routine inside FC_Manager.adb.
        G. Prete
        11/11/2004
    */    
    return FibreLockAda (HostID);
}

Boolean Fibre_Release(u8 HostID)
{
    /*  Call Ada routine inside FC_Manager.adb.
        G. Prete
        11/11/2004
    */    
    return FibreReleaseAda (HostID);
}


/*************************************************************************
*
*  FCDelay
*
*************************************************************************
*
* This function performs a counter delay
* This function can be modified to make use of application specific timer functions
* It should be designed to provide 5 usec of delay with the associated count of FC2100ResetDelay 
*
* Input : u32 Count
*
* Return: none
*
************************************************************************/
void FCDelay(u32 Count)
{
    volatile u32 Delay = Count;

    while (Delay > 0) Delay--;

    return;
}

