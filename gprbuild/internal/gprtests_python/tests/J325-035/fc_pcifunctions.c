/*

--**********************************************************************
--**                      Northrop Grumman                            **
--**     Information contained herein is proprietary to               **
--**                      Northrop Grumman                            **
--**********************************************************************
--
--                   AAA     SSSSS    CCCCC                      
--                  AA AA   SS   SS  CC   CC            
--                 AA   AA  SS       CC                   
--                 AAAAAAA   SSSSS   CC                  
--                 AA   AA       SS  CC                   
--                 AA   AA  SS   SS  CC   CC                 
--                 AA   AA   SSSSS    CCCCC                
--
--
-- Northrop Grumman Bethpage, New York 11714
--
-- Program      : EA-18G
--
-- CSCI         : ASC
--
-- DATE CREATED : 4/10/09
--
-- AUTHOR       : G. Prete
--
-- PURPOSE      : This package will contain functions to interface the
--                EAU OFP to the Fibre Channel PCI card.  This module
--                will utilize the vendor-supplied functions inside the BSP.
--
--
-- MODIFICATION HISTORY
--
--     AUTHOR        :  G. Prete
--     DATE          :  
--     MODIFICATION  :  
--                      
--                      
--                      
--      
-- ------------------------------------------------------------------------ --
-- ------------------------------------------------------------------------ --

*/



/************************************************************************/
/*          Initialize Fibre Channel PCI and Interrupts                 */
/************************************************************************/

unsigned int Initialize_FibreChannel_PCI 
   ( unsigned     PCIHose,
     unsigned int ConfigBaseAddress,
     int          PortID )

{

    /*  This function will initialize the Fibre Channel PCI device at 
        "ConfigBaseAddress" by setting all necessary values in the PCI
        Configuration Space Header.  It will then pass the Memory base
        address of the card back to the caller.
        
        G. Prete 
        Northrop Grumman Corporation
        Bethpage, L.I.   
        August 30. 2004
    */
}


void Clear_PCI_Device
   ( unsigned     PCIHose,
     unsigned int ConfigBaseAddress )

{

    /*  This function will clear the PCI command register for one
        FibreChannel port, thus disabling the port completely.
    */
}



/************************************************************************/
/*                         GetPCIConfigAddress                          */
/************************************************************************/


/*  The following record structure will be passed back to
    an Ada 95 calling program.
*/

typedef struct  
{
  unsigned  PCIHose;          /* Pointer to PCI0 or PCI1 space.   */
  unsigned  ConfigAddress;    /* Config base address of card.     */
  int       devNum;           /* Device Number.                   */
  int       Padding;          /* Padding MUST stay.  This struct
                                 has to be greater than 8 bytes!! */
} PCIConfigDataType;



PCIConfigDataType GetPCIConfigAddress
   (int FUNC, short VENDOR, short DEVICE, int deviceNumber) 
{
   /*
       Searches both PCI0 and PCI1 buses for a specific PCI device,
       and returns the bus it found it on, along with the Configuration
       Base Address.
       
       G. Prete 
       Northrop Grumman Corporation
       Bethpage, L.I.   
       October 21. 2004
       
   */
}




/************************************************************************/
/*                Disable 1553 Card and Interrupts                      */
/************************************************************************/

void Disable_1553_Chip
   ( unsigned     PCIHose,
     unsigned int ConfigBaseAddress )
{
}



/************************************************************************/
/*     Start new INTEGRITY image already loaded at bootArea             */
/************************************************************************/

void INTEGRITY_Restart (unsigned *bootArea)
{
}


/************************************************************************/
/*                     Flush / invalidate caches                        */
/************************************************************************/

void dcache_flush
   ( unsigned StartAddr,
     int      Length )
{
}


int ASP_FlushCaches
   ( unsigned Address,
     int      Size )
{
    return (0);
}


int ASP_InvalidateCaches
   ( unsigned Address,
     int      Size )
{
    return (0);
}


