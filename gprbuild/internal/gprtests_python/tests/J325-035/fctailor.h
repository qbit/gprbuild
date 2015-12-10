/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FCTAILOR.h   $Revision: 1.1 $
    Module Description:     Host specific definitions for the FCAPI

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
#ifndef FCTAILOR_H
#define FCTAILOR_H

/* inlcude system specific header files needed here. i.e. debug printf include file */

/* user may set both ISP2300 and ISP2322 flags to allow runtime determination of hardware platform. */ 
/*   or only ISP2300 or ISP2322 to create a smaller software image. 								*/
/* Define ISP2300 if  running on a 2300, 2310, or 2312.     */
/* Define ISP2322 if running on 2322 		                           */
/* Define ISP24XX if and only if running on 2422 or 2432.                          */
/* DO NOT define ISP24xx with either ISP2300 or ISP2322 flags set!!!!! */
/* Will not operate with 2200 or 2100 hardware. */

#if 1
 #define ISP2300
#endif
#if 1
 #define ISP2322        
#endif
#if 0
 #define ISP24XX
#endif

#if ((defined ISP2300 || defined ISP2322) && defined ISP24XX)
#error "Cannot build for both 24xx and 23xx hardware platforms!!!"
#endif


/* number of local adapters to support */

#define MAX_ADAPTERS 2                  /* The max number of host adapters to be handled, 126 is limit  */
                                        /* typically this number would be 1, lower number saves memory  */
                                        /* and execution time needed to locate appropriate adapter data */

/* modify these typedefs as required for the specific host system   */

typedef signed char     s8;     /* signed 8 bit value       */
typedef unsigned char   u8;     /* unsigned 8 bit value     */
typedef signed short    s16;    /* signed 16 bit value      */
typedef unsigned short  u16;    /* unsigned 16 bit value    */
typedef signed long     s32;    /* signed 32 bit value      */
typedef unsigned int   u32;    /* unsigned 32 bit value - some 32 bit compilers may require "unsigned int" */

typedef int Boolean;            /* boolean type             */


typedef u32 BaseAddressType;    /* use for device access    */

/* The following define statement is used to support big endian processors.  If the target   */
/* processor is a big endian processor, enable the following definition.  Otherwise, leave   */
/* the definition disabled.                                                                  */
/* The activation of the BIG_ENDIAN define statement will cause the compiler to re-organize  */
/* the firmware command data structures.  This re-organization in conjunction with the       */
/* routine BigEndConvert(...) will permit the big endian host processor to communicate with  */
/* the Qlogic firmware.                                                                      */

#if 1
   #define BIG_ENDIAN
#endif


/* The following define allows 64 bit addresses in scatter gather lists.                    */

#if 0
  #define SGL_64_BIT_SUPPORT    
#endif

#if 0
	#define CONT_IOCB_FOR_SG
#endif

/* The following define statement identifies how the compiler assigns bit-fields within a    */
/* u32 (unsigned 32-bit word).  Some compilers assign bits from high to low (in this case    */
/* enable the definition HIGH_2_LOW).  Other compilers assign bits from low to high (in this */
/* case disable the definition HIGH_2_LOW).                                                  */
/* This is typically disabled for both INTEL and Power PC platforms                          */

#if 0
#define HIGH_2_LOW
#endif

/* The follwing define statement identifies if the host processor is limited to data types  */
/* of only 32 bits.  It does not support 8 bit bytes or 16 bit shorts.  The SHARC DSP is one*/
/* example of this type of processor.                                                       */
#if 0
#define FIXED_32BIT
#endif

/* The following define is used when working with processors don't support byte addressing  */
/* (TI C40 et. al.)                                                                         */
#define ADDRESSING_SIZE 1       /* 1 for byte addressing, 2 for word addressing and         */
                                /* 4 for long word addressing                               */   




/* The following define statements will permit the FCRM to collect debug information.        */
/* The data collection method is specified by FCDEBUGPRINT.  The data collection method must */
/* be executable from within an ISR (i.e. setting FCDEBUGPRINT to printf would not work).    */
/* Two of the source code modules, fc2100.c and fcscsi.c, use the FCDEBUGPRINT utility.  The */
/* FCDEBUGPRINT utility has a calling prototype equivalent to printf.                        */
/*                                                                                           */
/* Developer applications that employ a polling approach may use printf for the FCDEBUGPRINT */
/* data collection method.                                                                   */

/* Define the data collection method.        */
#define FCDEBUGPRINT(args) /*printf args*/    

/* The following define enables descriptor list checking.  If the desctiptor list scatter    */
/* gather buffer byte count (i.e. the sum of the buffer sizes on the descriptor list)        */
/* doesn't match the total byte count (BufferSize in the TransferInfoType record), the       */
/* data transfer will be corrupted.                                                          */
/* A conditional compile define was created to support integration testing.  There is a real */
/* time penalty with this conditional compile, each time a scatter/gather transfer is        */
/* requested, the FCRM will check the descriptor list.                                       */

#if 0
#define CHECK_DESCRIPTOR_LIST
#endif


/* Enable if you want to check your system for synchronization problems - ie interrupting or calling
   an API function - Place Breakpoint in CheckSynchronization in FC2100.c
*/

#if 0
#define CHECK_SYNCHRONIZATION
#endif

/* define for Cache Management - primarily for DMA of RISC code */
#if 1
#define MANAGE_CACHE
#endif
/* define for IP support  */  
#if 0
#define IP_SUPPORT
#endif

#if 1
#define ENABLE_AMCD
#endif

/* if prequeued target transfers are not use, enable this to speed target processing */
/* it disables the search for prequeued entries                                      */
#if 0
#define DISABLE_PREQUEUED_SUPPORT
#endif

/* the following define is used to make working with a non-cached working buffer more efficient */
/* only enable if the working buffer is allocated out of non-cached memory                      */
#if 0
#define NON_CACHED_WORKING_BUFFER
#endif

/* The following define is used to turn on managmenet of cache when the system does not     */
/* support cache coherency via bus snooping.                                                */
/*  Fibre_Flush_Cache() and Fibre_Invalidate_Cache() must be defined properly               */
/* These functions are defined as macros to speed execution                                 */
#if 1
#define FIBRE_NONCOHERENT_CACHE_MANAGEMENT
#endif 



/************** DO NOT MODIFY THESE SETTINGS ******************************************/
/* The following logic established the firmware interface.  The 32-bit interface requires the
 * FCRM to accommodate for compilers that assign bit fields "high to low" and "low to high".
 * The FCRM also accommodates big and little endian memory interfaces.  These two options
 * produce a total of 4 options.  It turns out that there is overlap in these options that
 * require the FCRM to support only two structure definitions.  This header file evaluates
 * the conditional flags BIG_ENDIAN and HIGH_2_LOW (provided by FCTAILOR.H) to identify
 * the firmware interface data structure format (as either STRUCT_CASE1 or not).
 * The flag STRUCT_CASE1 is used in FCIOCB.H and FCMBCMDS.H. and FCAPI.H
 * */
/************** DO NOT MODIFY THESE SETTINGS ******************************************/

#ifndef BIG_ENDIAN
#   ifndef HIGH_2_LOW
#       define STRUCT_CASE1
#    endif
#else
#   ifdef HIGH_2_LOW
#       define STRUCT_CASE1
#    endif
#endif
#ifdef FIBRE_NONCOHERENT_CACHE_MANAGEMENT 
/*************************************************************************
*
*  Macro Fibre_Flush_Cache(u8 *Address, u32 Size)
*
*************************************************************************
*
* This function will flush the cache lines associate with the specified data buffer 
*  
*
* Input : u8 *Address - The address of the start of the data buffer that needs to be flushed.
*         u32 Size    - The length of the data buffer to be flushed.
*
* Return: none
*
************************************************************************/
#define Fibre_Flush_Cache(Address, Size)  ASP_FlushCaches((unsigned)Address, Size)


/*************************************************************************
*
*  Macro Fibre_Invalidate_Cache(u8 *Address, u32 Size)
*
*************************************************************************
*
* This function will Invalidate the cache lines associated with the specified data buffer 
*  
*
* Input : u8 *Address - The address of the start of the data buffer that needs to be invalidate.
*         u32 Size    - The length of the data buffer to be invalidated.
*
* Return: none
*
************************************************************************/
#define Fibre_Invalidate_Cache(Address, Size)  ASP_InvalidateCaches((unsigned)Address, Size)

#endif /* FIBRE_NONCOHERENT_CACHE_MANAGEMENT */


#endif /* FCTAILOR_H */
