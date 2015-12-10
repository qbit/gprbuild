/*
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
  ************************************************************************
*******************************************************************************************
*/

#include "fccommon.h"
#include <stdio.h>

void Fibre_Send_Trace_Debug_Addr(HostAdapterType *HostAdapter);

/* *******************************************************************************************
    Fibre_Set_Timeouts

    Description:
        Allows the host to specify FC Timeout values 
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HOST_ID           : The Fibre Channel ID of adapter where the transfer was assigned
                u32     CommandID               : The Command ID for the Callback function
                u16 AL_TIME                     : The time specified by user in ms
                u16 R_T_TOV                     : The time specified by user in ms
                u16 E_D_TOV                     : The time specified by user in ms
                u16 LP_TOV                      : The time specified by user in ms
                void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
		  E_D_TOV converted to 5ms base for use in f/w.  All other timeouts used in ms by f/w.
		  Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Set_Timeouts(u8 HOST_ID, u32 CommandID, u16 FC_AL_TIME, u16 FC_E_D_TOV, u16 FC_R_T_TOV, u16 FC_LP_TOV, 
										void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
	int rem;

    if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];

		FC_E_D_TOV = (int)FC_E_D_TOV/5;			/* 5ms base value*/
		rem = (int)FC_E_D_TOV%5;
		if (rem > 2) FC_E_D_TOV=FC_E_D_TOV+5;				/* round up*/

        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = HostSetTOVsSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_HOST_SET_TOVS;			
            MBCQueueItem.Mailbox[1]            = FC_AL_TIME;			
            MBCQueueItem.Mailbox[2]            = FC_R_T_TOV;			
            MBCQueueItem.Mailbox[3]            = FC_E_D_TOV;			
            MBCQueueItem.Mailbox[6]            = FC_LP_TOV;			
            MBCQueueItem.NumMailboxRegs        = 5;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = HostSetTOVsSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_HOST_SET_TOVS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, FC_AL_TIME);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, FC_R_T_TOV);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, FC_E_D_TOV);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, FC_LP_TOV);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Get_Login_Parameters

    Description:
        Provides access to port/fabric login parameters  
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HOST_ID           : The Fibre Channel ID of adapter where the transfer was assigned
        u8 PortID                       : The Fibre Channel ID of desired port
        u32 LoginParametersAddress  : Physical memory where data is to be stored
        u32 CommandID           : User specified ID for callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Get_Login_Parameters(u8 HOST_ID, u16 PortID, u32 LoginParametersAddress, 
                                              u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID)) 
/*
FIBRE_TRANSFER_ERR Fibre_Get_Login_Parameters(u8 HOST_ID, u8 PortID, LoginInfoType *LoginParametersBuff, 
                                              u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID))
*/                                               
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;

    if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetLoginParamsSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_PORT_DB;			
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[1]            = PortID;            
            else
            	MBCQueueItem.Mailbox[1]            = (PortID << 8) & 0xFF00;			
            MBCQueueItem.Mailbox[2]            = HostAdapter->MBCmdBufferPA  >> 16;			
            MBCQueueItem.Mailbox[3]            = HostAdapter->MBCmdBufferPA  & 0xFFFF;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            MBCQueueItem.MBCQLoginParamBuffPointer = (LoginInfoType *) LoginParametersAddress;
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = GetLoginParamsSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
		HostAdapter->MBCLoginParamBuffPointer = (LoginInfoType *) LoginParametersAddress;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_PORT_DB);
        if (0)// (HostAdapter->DevType >= FD_ISP_2322)
        	FCWrite16(HostAdapter->BaseAddress + Mailbox1, PortID);
        else
        	FCWrite16(HostAdapter->BaseAddress + Mailbox1, (PortID << 8) & 0xFF00);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Get_Errors

    Description:
        Provides access to fibre channel errors for host port  
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HOST_ID           : The Fibre Channel ID of adapter where the transfer was assigned
        u32 ErrorLogAddress  : Physical memory where data is to be stored
        u32 CommandID           : User specified ID for callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Get_Errors(u8 HOST_ID, ErrorLogType *ELOG, u32 CommandID, 
                                    void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;


    if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetFibreErrorsSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_ERRORS;			
            MBCQueueItem.Mailbox[1]            = HostAdapter->MBCmdBufferPA  >> 16;			
            MBCQueueItem.Mailbox[2]            = HostAdapter->MBCmdBufferPA  & 0xFFFF;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            MBCQueueItem.MBCQUserErrLogPointer = ELOG;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = GetFibreErrorsSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        HostAdapter->MBCUserErrLogPointer  = ELOG;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_ERRORS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, HostAdapter->MBCmdBufferPA  >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA  & 0xFFFF);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Request_LESB

    Description:
        Allows host port to request LESB's for specified target port
        Returns FIBRE_TRANSFER_ERR type
        
    Parameters:
        u8 HOST_ID           : The Fibre Channel ID of adapter where the transfer was assigned
        u8 PortID                       : The Fibre Channel ID of desired port
        u32 ErrorLogAddress  : Physical memory where data is to be stored
        u32 CommandID           : User specified ID for callback function
        void (*NotificationHandler)(TransferInfoType *)): Callback function to handle LLP target completion notifications
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Request_LESB(u8 HOST_ID, u16 PortID, u16 SwitchID, LESBType *LESB, u32 CommandID, 
                                      void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    
        if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetLESBSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_GET_LINK_STATUS;			
            if (0)// (HostAdapter->DevType >= FD_ISP_2322)
                MBCQueueItem.Mailbox[1]            = PortID;            
            else
	            MBCQueueItem.Mailbox[1]            = (PortID << 8) & 0xFF00;			
            MBCQueueItem.Mailbox[2]            = HostAdapter->MBCmdBufferPA  >> 16;			
            MBCQueueItem.Mailbox[3]            = HostAdapter->MBCmdBufferPA  & 0xFFFF;			
            MBCQueueItem.Mailbox[6]            = SwitchID;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 6;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            MBCQueueItem.MBCQUserLESBPointer   = LESB;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = GetLESBSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        HostAdapter->MBCUserLESBPointer	 = LESB;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_GET_LINK_STATUS);
        if (0)// (HostAdapter->DevType >= FD_ISP_2322)
	        FCWrite16(HostAdapter->BaseAddress + Mailbox1, PortID);
	    else
	        FCWrite16(HostAdapter->BaseAddress + Mailbox1, (PortID << 8) & 0xFF00);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->MBCmdBufferPA >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->MBCmdBufferPA);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, SwitchID);
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}
/* AMCD dfa - 06/29/01 */
/* *******************************************************************************************
    Fibre_Data_Rate

    Description:
        Request to set DATA RATE for fibre channel.  
        Returns FIBRE_TRANSFER_ERR type
		
		Mailbox0 - opcode (005Dh)
        Mailbox1 - 0: Get current data rate
			   1: Set data rate (as specified in mailbox 2)
			   2: Set data rate (as specified in mailbox 2) forces LOS, re-starts Loop/Link Init 
		Mailbox2 - 0: 1 Gbit fixed data rate
			   1: 2 Gbit fixed data rate
			   2: Autonegotiated data rate
        
        
    Parameters:
        u8 HOST_ID           : The Fibre Channel ID of adapter where the transfer was assigned
        u8 PortID                       : The Fibre Channel ID of desired port
        u32 CommandID           : User specified ID for callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR:       Return status -- The call back function will not be
                                                   called be if error status is returned here.                  

    Notes:
        Callback parameters:
            Status
                Bits 0-7:
                    0 = Success
                    1 = Fail
                    2-255 - Reserved.
                
            ID - the 32 bit ID assigned to this command

******************************************************************************************* */
FIBRE_TRANSFER_ERR Fibre_Data_Rate(u8 HOST_ID, u32 CommandID, 
                                    void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;


    if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = DataRateSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_DATA_RATE;			
            MBCQueueItem.Mailbox[1]            = 2;			
            MBCQueueItem.Mailbox[2]            = 2;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = DataRateSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_DATA_RATE);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, 2);  /* see above*/
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, 2);  /* see above*/
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* 06/01/01 - dfa */
/* *******************************************************************************************
    Fibre_Send_CSR

    Description:
        Initiates a Clock Sync Request (CSR) ELS when commanded by the UGS. 

    Parameters:
        u8  HOST_ID:         Adapter ID
        u32 CS_mode:        Clock Sync Mode data(mode,mantissa,expon,MSB,LSB) 
        u32 CS_update:      Clock Sync update period 
        u32 CommandID:      The Command ID for the Callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: Status structure

    Notes:  This initiates a CSR message to the server at WKA 'FFFFf6'.
            This will initiate a Clock Sync Update from the server clock.

******************************************************************************************* */
 
FIBRE_TRANSFER_ERR Fibre_Send_CSR(u8 HostID, u8 PortID, u32 CS_mode, u32 CS_update,
                                  u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
 
    if (HostID > PORT_ID_MAX)              /* validate host id                      */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    /* locate adapter in adapter list */
    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)   /* adapter not in list */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter is initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = InitiateCSRSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_REQUEST_CSR;			
            MBCQueueItem.Mailbox[1]            = (PortID << 8) & 0xFF00;			
            MBCQueueItem.Mailbox[2]            = (CS_mode >> 16);			
            MBCQueueItem.Mailbox[3]            = (CS_mode & 0x0000ffff);			
            MBCQueueItem.Mailbox[6]            = (CS_update >> 16);			
            MBCQueueItem.Mailbox[7]            = (CS_update & 0x0000ffff);			
            MBCQueueItem.NumMailboxRegs        = 6;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
		
        HostAdapter->MBCState            = InitiateCSRSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_REQUEST_CSR);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, (PortID << 8) & 0xFF00);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, (CS_mode >> 16));
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, (CS_mode & 0x0000ffff));
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, (CS_update >> 16));
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, (CS_update & 0x0000ffff));
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HostID);
    /* Mutual exclusion ends here   */
    
    return Status;
    
} /* end Fibre_Send_CSR */

#if 1
/* 09/19/01 - dfa */
/* *******************************************************************************************
    Fibre_Send_CSU

    Description:
    TEST ONLY - Sends CSU NIC to NIC 
        Initiates a Clock Sync Update (CSU) ELS when commanded by the UGS. Utilizes test
        firmware which is modified to send a CSU vs a CSR to the selected port using the 
        existing (modified for test) Send CSR mailbox functions in FW. The receiving port
        will notify it's host via an Async Event (normal path).

    Parameters:
        u8  HOST_ID:         Adapter ID
        u8  PortID:         Target ID
        u32 CommandID:      The Command ID for the Callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: Status structure


******************************************************************************************* */
 
FIBRE_TRANSFER_ERR Fibre_Send_CSU(u8 HostID, u8 PortID, u8 *CSU_data, 
                                  u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
 
    if (HostID > PORT_ID_MAX)              /* validate host id                      */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    /* locate adapter in adapter list */
    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)   /* adapter not in list */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter is initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = InitiateCSUSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_SEND_CSU;			
            MBCQueueItem.Mailbox[1]            = (PortID << 8) & 0xFF00;			
            MBCQueueItem.Mailbox[2]            = *(u16*)CSU_data;			
            MBCQueueItem.Mailbox[3]            = *(u16*)(CSU_data+2);			
            MBCQueueItem.Mailbox[6]            = *(u16*)(CSU_data+4);			
            MBCQueueItem.Mailbox[7]            = *(u16*)(CSU_data+6);			
            MBCQueueItem.NumMailboxRegs        = 6;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
		
        HostAdapter->MBCState            = InitiateCSUSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;

        /* Send TEST Clock Sync Update to NIC port */
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SEND_CSU);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, (PortID << 8) & 0xFF00);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, *(u16*)CSU_data);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, *(u16*)(CSU_data+2));
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, *(u16*)(CSU_data+4));
        FCWrite16(HostAdapter->BaseAddress + Mailbox7, *(u16*)(CSU_data+6));
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HostID);
    /* Mutual exclusion ends here   */
    
    return Status;
    
} /* end Fibre_Send_CSU */
#endif

/* 08/10/01 - dfa */
/* *******************************************************************************************
    Fibre_Get_RTC

    Description:
        Initiates a Request RTC from Risc ELS when commanded by the UGS. 

    Parameters:
        u8  HOST_ID:         Adapter ID
        u32 CommandID:      The Command ID for the Callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: Status structure


******************************************************************************************* */

 
FIBRE_TRANSFER_ERR Fibre_Get_RTC(u8 HostID, u32 RTCLogAddress, u32 CommandID, 
                                        void (*CallBackFunction)(u32 Status, u32 ID))
{
    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;

    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
 
    if (HostID > PORT_ID_MAX)              /* validate host id                      */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HostID))
        return FT_INVALID_HOST_ID;

    /* locate adapter in adapter list */
    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HostID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)   /* adapter not in list */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter is initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = GetRTCSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_REQUEST_RTC;			
            MBCQueueItem.Mailbox[1]            = RTCLogAddress >> 16;			
            MBCQueueItem.Mailbox[2]            = RTCLogAddress & 0xFFFF;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HostID);
            return Status;
        }
		
        HostAdapter->MBCState            = GetRTCSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;

        /* initiate Get RTC from RISC */
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_REQUEST_RTC);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, RTCLogAddress >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, RTCLogAddress & 0xFFFF);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HostID);
    /* Mutual exclusion ends here   */
    
    return Status;
    
} /* end Fibre_Get_RTC */

/* *******************************************************************************************
    Fibre_Set_Test_Flags

    Description:
        Sets a flag(s) to disable or modify target responses. 

    Parameters:
        u8  HOST_ID:         Adapter ID
        u32 CommandID:      The Command ID for the Callback function
        u16 TestFlags		Flags modifying specific target behavior to be tested
		void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: Status structure


******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Set_Test_Flags(u8 HOST_ID, u32 CommandID, u16 TestFlags, u16 TestFlags2,  
                                      void (*CallBackFunction)(u32 Status, u32 FlagsSet))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    
        if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = SetTestFlagsSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_SET_TEST_FLAGS;			
            MBCQueueItem.Mailbox[1]            = TestFlags;			
            MBCQueueItem.Mailbox[2]            = TestFlags2;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = SetTestFlagsSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SET_TEST_FLAGS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, TestFlags);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, TestFlags2);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}
/* *******************************************************************************************
    Fibre_Register_Debug_Addr

    Description:
        Send a command to BBS debugger 

    Parameters:
        u8  HOST_ID:         Adapter ID
        u16 Command			 debug command
		u16 Addr			 any associated data (bp addr, ram addr)
        u16 Len				 length of RAM read 
        u32 CommandID:      The Command ID for the Callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: Status structure


******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Register_Debug_Addr(u8 HOST_ID, u32 DbgBuffPA, u32 CommandID, 
                                      void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    
        if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = RegDbgRetAddrSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_SET_TEST_FLAGS;			
            MBCQueueItem.Mailbox[1]            = DbgBuffPA & 0xFFFF;			
            MBCQueueItem.Mailbox[2]            = DbgBuffPA >> 16;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 3;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            MBCQueueItem.MBCDbgDataBuff        = DbgBuffPA;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = RegDbgRetAddrSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        HostAdapter->DbgDataBuff         = DbgBuffPA;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_REG_DBG_ADDR);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, DbgBuffPA & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, DbgBuffPA >> 16);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Register_Debug_Addr

    Description:
        Send a command to BBS debugger 

    Parameters:
        u8  HOST_ID:         Adapter ID
		u32 Addr			 buffer address for dumping trace data
        u32 BuffSize			 Size of buffer in Bytes 
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: Status structure


******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Register_Trace_Debug_Addr(u8 HOST_ID, u32 DbgBuffPA, u32 BuffSize, 
                                      u32 CommandID, void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    u16 DumpSize = 512;/* Size in Bytes */
        if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = RegDbgRetAddrSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_REG_TRC_ADDR;			
            MBCQueueItem.Mailbox[1]            = DbgBuffPA & 0xFFFF;			
            MBCQueueItem.Mailbox[2]            = DbgBuffPA >> 16;			
            MBCQueueItem.Mailbox[3]            = BuffSize & 0xffff;			
            MBCQueueItem.Mailbox[6]            = BuffSize >> 16;			
            MBCQueueItem.NumMailboxRegs        = 5;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            MBCQueueItem.MBCDbgDataBuff        			= DbgBuffPA;       	
            MBCQueueItem.MBCDbgDmpTrcBuffBasePA        	= DbgBuffPA;       	
            MBCQueueItem.MBCDbgDmpTrcSize      			= DumpSize;       	
            MBCQueueItem.MBCDbgDmpTrcCnt       			= 0;       	
            MBCQueueItem.MBCDbgDmpNumTrcWords  			= BuffSize;       	
            MBCQueueItem.MBCDbgDmpNumTrcBuffsRegd       = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
        HostAdapter->MBCCallBackFunction 		= CallBackFunction;
        HostAdapter->MBCClientID         		= CommandID;
		HostAdapter->DbgDmpTrcBuffBasePA		= DbgBuffPA;
		HostAdapter->DbgDmpTrcBuffPtr			= DbgBuffPA;
		HostAdapter->DbgDmpTrcSize				= DumpSize; 
		HostAdapter->DbgDmpTrcCnt				= 0; 
		HostAdapter->DbgDmpNumTrcWords			= BuffSize;
		HostAdapter->DbgDmpNumTrcBuffsRegd		= 0;
        HostAdapter->MBCCallBackFunction        = CallBackFunction;
		Fibre_Send_Trace_Debug_Addr(HostAdapter);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Register_Debug_Addr

    Description:
        Send a command to BBS debugger 

    Parameters:
		HostAdapterType *HostAdapter
    Return Value:
        FIBRE_TRANSFER_ERR: Status structure


******************************************************************************************* */

void Fibre_Send_Trace_Debug_Addr(HostAdapterType *HostAdapter)
{
    if (!(FCRead16(HostAdapter->BaseAddress + IspHccr) & HCTLH2RINTR)) /* see if 2100 thinks it is processing a cmd */    
	{
        HostAdapter->MBCState            = RegDbgTrcAddrSent;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_REG_TRC_ADDR);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, HostAdapter->DbgDmpTrcBuffPtr & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, HostAdapter->DbgDmpTrcBuffPtr >> 16);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, HostAdapter->DbgDmpNumTrcWords & 0xFFFF);
        FCWrite16(HostAdapter->BaseAddress + Mailbox6, HostAdapter->DbgDmpNumTrcWords >> 16);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    } /* end else host adapter initialized */
    else
	    HostAdapter->Flags.ReqSendDbgTrcAddr  	 = FC_TRUE; /* make sure we request it later*/
            
    return;
}
/* *******************************************************************************************
    Fibre_Send_Debug_Cmd

    Description:
        Send a command to BBS debugger 

    Parameters:
        u8  HOST_ID:         Adapter ID
        u16 Command			 debug command
		u16 Addr			 any associated data (bp addr, ram addr)
        u16 Len				 length of RAM read 
        u32 CommandID:      The Command ID for the Callback function
        void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_TRANSFER_ERR: Status structure


******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Send_Debug_Cmd(u8 HOST_ID, u16 Command, u16 Addr, u16 Len, u32 CommandID, 
                                      void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    
        if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = SendDebugCmdSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_SEND_DEBUG_CMD;			
            MBCQueueItem.Mailbox[1]            = Command;			
            MBCQueueItem.Mailbox[2]            = Addr;			
            MBCQueueItem.Mailbox[3]            = Len;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 4;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            MBCQueueItem.MBCDbgDataLen         = Len;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = SendDebugCmdSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;
        HostAdapter->DbgDataLen          = Len;
        
        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SEND_DEBUG_CMD);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, Command);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, Addr);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, Len);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}

/* *******************************************************************************************
    Fibre_Set_AdditFWOpts

    Description:
        Enables additional AE reporting due to LIP errors/TO's. 

    Parameters:
        u8  HOST_ID:         Adapter ID
        u32 CommandID:      The Command ID for the Callback function
        u16 AddOPts:		Bit 7 set to enable AE reporting
		void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_INIT_ERR: Status structure


******************************************************************************************* */

FIBRE_INIT_ERR Fibre_Set_AdditFWOpts(u8 HOST_ID, u32 CommandID, u16 AddOpts, 
										void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    
        if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = SetAddOptsSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_SET_TARGET_PARAMS;			
            MBCQueueItem.Mailbox[1]            = AddOpts;			
            MBCQueueItem.Mailbox[2]            = 0;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 4;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = SetAddOptsSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SET_TARGET_PARAMS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, AddOpts);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}


/* *******************************************************************************************
    Fibre_Set_AdditFWOpts

    Description:
        Enables additional AE reporting due to LIP errors/TO's. 

    Parameters:
        u8  HOST_ID:         Adapter ID
        u32 CommandID:      The Command ID for the Callback function
        u16 AddOPts:		Bit 7 set to enable AE reporting
		void (*CallBackFunction)(u32 Status, u32 ID): Pointer to the Callback Function

    Return Value:
        FIBRE_INIT_ERR: Status structure


******************************************************************************************* */

FIBRE_TRANSFER_ERR Fibre_Set_CS_FPGA_Opts(u8 HOST_ID,  u16 Opts, u32 CommandID, 
										void (*CallBackFunction)(u32 Status, u32 ID))
{

    u8 HostAdapterIndex;
	u16 Options;
    HostAdapterType *HostAdapter;
    FIBRE_TRANSFER_ERR Status = FT_SUCCESS;
    
        if (HOST_ID > PORT_ID_MAX)                                       /* validate host id                 */
        return FT_INVALID_HOST_ID;

    /* Mutual exclusion starts here */
    if (!Fibre_Lock(HOST_ID))
        return FT_INVALID_HOST_ID;

    for (HostAdapterIndex = 0; HostAdapterIndex < MAX_ADAPTERS; HostAdapterIndex++)  /* locate adapter in adapter list */
        if (HostAdapters[HostAdapterIndex].Initialized && HostAdapters[HostAdapterIndex].PortID == HOST_ID)
            break;

    if (HostAdapterIndex >= MAX_ADAPTERS)                           /* adapter not in list              */
        Status = FT_DEVICE_NOT_INITIALIZED;
    else /* host adapter initialized */
    {
        HostAdapter = &HostAdapters[HostAdapterIndex];
        if (Opts & USE_GPIO_EN_FLAG) 
        	Options = 0x0003;  
        else if (Opts & FIFO_SNOOP_EN_FLAG)
        	Options = 0x0002;
        else
        	Options = 0x0;	
		
		if (Opts & GPIO_LD_ZERO_FLAG)
				Options |= GPIO_LD_ZERO;
        else if (Opts & GPIO_LD_PAT1_FLAG)
				Options |= GPIO_LD_PAT1;
        else if (Opts & GPIO_LD_PAT2_FLAG)
				Options |= GPIO_LD_PAT2;
        else if (Opts & GPIO_LD_PAT3_FLAG)
				Options |= GPIO_LD_PAT3;

        if (HostAdapter->MBCState != NoMBCSent)                          /* if currently processing a cmd return */
        {
            MBCQueueItemType MBCQueueItem; 

            MBCQueueItem.MBCState              = SetCsFpgaOptsSent;				
            MBCQueueItem.Callback              = CallBackFunction;;				
            MBCQueueItem.ClientID              = CommandID;             
            MBCQueueItem.Mailbox[0]            = MBC_SET_CS_FPGA_OPTS;			
            MBCQueueItem.Mailbox[1]            = Options;			
            MBCQueueItem.Mailbox[2]            = 0;			
            MBCQueueItem.Mailbox[3]            = 0;			
            MBCQueueItem.Mailbox[6]            = 0;			
            MBCQueueItem.Mailbox[7]            = 0;			
            MBCQueueItem.NumMailboxRegs        = 2;       
            MBCQueueItem.AbortingTargetID      = 0;  	
            MBCQueueItem.ResetingLUN           = 0;       	
            
            if (MBCQueueInsertItem(HostAdapter, &MBCQueueItem) != MBCQueueSuccess)
                Status = FT_OUTSTANDING_REQUEST;
            Fibre_Release(HOST_ID);
            return Status;
        }
		
        HostAdapter->MBCState            = SetCsFpgaOptsSent;
        HostAdapter->MBCCallBackFunction = CallBackFunction;
        HostAdapter->MBCClientID         = CommandID;
        HostAdapter->AbortingTargetID    = 0;
        HostAdapter->ResetingLUN         = 0;

        FCWrite16(HostAdapter->BaseAddress + Mailbox0, MBC_SET_CS_FPGA_OPTS);
        FCWrite16(HostAdapter->BaseAddress + Mailbox1, Options);
        FCWrite16(HostAdapter->BaseAddress + Mailbox2, 0);
        FCWrite16(HostAdapter->BaseAddress + Mailbox3, 0);
        SET_HOST2RISC_INTR(HostAdapter->BaseAddress);
    
    } /* end else host adapter initialized */
            
    Fibre_Release(HOST_ID);

    /* Mutual exclusion ends here   */
    
    return Status;
}


