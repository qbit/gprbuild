/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FC_Q_MON.C    $Revision: 1.1 $
    Module Description:     Transfer information queue


  Special Notes:


  Revision History
    Date        Description of Change
    ---------   ------------------------
    7/10/98     Version 1.0
    8/28/98     Version 2.0 See rel2_0.txt
    10/5/98     Version 2.1 See rel2_1.txt
    3/19/99     Version 2.2 See rel2_2.txt
    5/20/99     Version 2.3 See rel2_3.txt
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
******************************************************************************************* */


#include "fccommon.h"
#if !defined(_FCAPI_PLATFORM_LINUX_) && !defined(_BBF_SOLARIS_CLIENT_)
#include <stdlib.h>
#endif
/*****************************************************************************************
Local Data
******************************************************************************************/

/*****************************************************************************************
Exported Routines
******************************************************************************************/

/****************************************************************************************
 *
 * function void init_transfer_queues(void)
 *     initializes the resource manager to have NULL pointers for the heads of the
 *     send and receive linked lists.  The free linked list head points to the first
 *     element in the transfer list.  Each transfer list element points to the subsequent
 *     element.  The last transfer list element next (pointer) is set to NULL.
 *
 *     All transfer list elements are on the free list.
 *
 *
 **************************************************************************************/
void init_transfer_queues(HostAdapterType *Host)
{
int i;   /* loop index variable */

    Host->TransferRecord = (transfer_record_type *) Fibre_Malloc(NUM_TRANSFER_RECORDS * sizeof(transfer_record_type));
                                                    /* freed in fibre_close */
    Host->TransferMgr.t_send_q_head    = FC_NULL;
    Host->TransferMgr.t_receive_q_head = FC_NULL;
    Host->TransferMgr.i_send_q_head    = FC_NULL;
    Host->TransferMgr.i_receive_q_head = FC_NULL;
    Host->TransferMgr.free_head        = &Host->TransferRecord[0];

    for (i = 0; i < NUM_TRANSFER_RECORDS - 1; i++)
        Host->TransferRecord[i].next = &Host->TransferRecord[i+1];

    Host->TransferRecord[NUM_TRANSFER_RECORDS - 1].next = FC_NULL;

    Host->TransferMgr.free_count = NUM_TRANSFER_RECORDS;
}


/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE insert_q_item(...)
 *      If a free transfer record is available (i.e. if an element is on the free linked list,
 *      the TransferInfoType data and handle are loaded into the free element.  This function
 *      identifies which linked list the record belongs to (based on transfer protocol type
 *      and communication direction).  The record is placed at the end of the selected linked
 *      list and the record will be removed from the free list.
 *
 *      After the function has successfully placed the record on the correct linked list,
 *      the function returns a FC_SUCCESS.
 *
 *      If no free elements are available - the function returns a FC_Q_FULL.
 *
 *      If the ReqQ_Entry pointer is not NULL, then the data is copied into record otherwise
 *      the EntryType field is set to 0.
 *
 *      During the search for the end of a linked list, if the function passes through more
 *      than NUM_TRANSFER_RECORDS records:
 *          - the function does not remove the record from the free list
 *          - the selected linked list is not altered
 *          - the value of FC_Q_CORRUPT is returned to the calling routine
 *
  **************************************************************************************/
FC_Q_ERR_TYPE insert_q_item(HostAdapterType *Host,
                            u32 handle,
                            TransferInfoType *TI, tCTIOType2IOCB *ReqQ_Entry)
{
int i;   /* counter variable to check for a corrupted linked list */

transfer_record_type *new_element;     /* element that receives the transfer info data    */
transfer_record_type *locator;         /* locates the tail of the linked list where the   */
                                       /* new record will be placed                       */

    if (FC_NULL == Host->TransferMgr.free_head)  return(FC_Q_FULL);

    /* Move data into a free element */
    new_element = Host->TransferMgr.free_head;

    new_element->TI = *TI;      /*  copy info into queue item */
    if (ReqQ_Entry)
        new_element->ReqQItem = *ReqQ_Entry;
    else
        new_element->ReqQItem.Header.EntryType = 0;     /* no entry tag */
    new_element->handle = handle;

    /*  Identify which linked list will receive the new element.  Also handle            */
    /*  the special case processing when the new element is placed at the                */
    /*  the beginning of a linked list.                                                  */

    if (TI->Direction < FC_RECEIVE)                     /* if send      */
    {
        if (TI->TransferProtocol >= FC_TARGET)          /* if Target    */
        {
            if (FC_NULL == Host->TransferMgr.t_send_q_head)
            {
                Host->TransferMgr.t_send_q_head = new_element;
                Host->TransferMgr.free_head     = new_element->next;
                new_element->next               = FC_NULL;

                Host->TransferMgr.free_count--;

                return FC_Q_SUCCESS;
            }
            else
                locator = Host->TransferMgr.t_send_q_head;
        }
        else
        {
            if (FC_NULL == Host->TransferMgr.i_send_q_head)
            {
                Host->TransferMgr.i_send_q_head = new_element;
                Host->TransferMgr.free_head     = new_element->next;
                new_element->next               = FC_NULL;

                Host->TransferMgr.free_count--;

                return FC_Q_SUCCESS;
            }
            else
                locator = Host->TransferMgr.i_send_q_head;
        }

    }
    else
    {
        if (TI->TransferProtocol >= FC_TARGET)
        {
            if (FC_NULL == Host->TransferMgr.t_receive_q_head)
            {
                Host->TransferMgr.t_receive_q_head = new_element;
                Host->TransferMgr.free_head        = new_element->next;
                new_element->next                  = FC_NULL;

                Host->TransferMgr.free_count--;

                return FC_Q_SUCCESS;
            }
            else
                locator = Host->TransferMgr.t_receive_q_head;
        }
        else
        {
            if (FC_NULL == Host->TransferMgr.i_receive_q_head)
            {
                Host->TransferMgr.i_receive_q_head = new_element;
                Host->TransferMgr.free_head        = new_element->next;
                new_element->next                  = FC_NULL;

                Host->TransferMgr.free_count--;

                return FC_Q_SUCCESS;
            }
            else
                locator = Host->TransferMgr.i_receive_q_head;
        }

    }

    /* Start searching the designated linked list for the end element.                   */
    /* Keep track of the number of links to monitor for a corrupted linked list.         */

    i = 0;
    while (FC_NULL != locator->next)
    {
        if (++i > NUM_TRANSFER_RECORDS)
            return (FC_Q_CORRUPT);
        locator = locator->next;
    }

    /* Now that the end of the list has been located, insert the new element at the      */
    /* end of the list and update the free list.                                       */

    locator->next = new_element;

    Host->TransferMgr.free_head = new_element->next;

    new_element->next = FC_NULL;

    Host->TransferMgr.free_count--;

    return(FC_Q_SUCCESS);

}


/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE *find_q_item(...)
 *     Identifies which linked list to traverse to locate a transfer record with the
 *     designated handle.  The linked list is identified by the transfer protocol and
 *     communication direction.  This routine copies the data into the result structure
 *     and  removes the record from one of the four
 *     transfer linked list,  and moves the record to the free list.
 *
 *     Special processing is performed if the record is the first element of one of the
 *     transfer linked lists.
 *
 *     If the record is located, accessed and transferred to the free list this function
 *     returns a value of FC_Q_SUCCESS.
 *
 *     If the record is not located, the linked lists are not updated and the function
 *     returns a value of FC_Q_NOT_FOUND.
 *
 *     If the search for the designated handle requires the function to traverse more than
 *     NUM_TRANSFER_RECORDS, the linked lists are not updated and the function returns a
 *     value of FC_Q_CORRUPT.
 *
 *     returns the pended Request Queue entry if present otherwise ReqQ_Entry is undefined
 **************************************************************************************/
FC_Q_ERR_TYPE find_q_item(HostAdapterType *Host,
                          FC_DIRECTION_TYPE direction,
                          ProtocolType protocol,
                          u32 handle,
                          TransferInfoType *result, tCTIOType2IOCB *ReqQ_Entry)
{
int i;  /* counter variable to keep track of the number of links traversed. */

transfer_record_type *locator = FC_NULL;  /* identifies the record prior to the one */
                                          /* containing the data                    */
transfer_record_type *record = FC_NULL;   /* points to the record containg the data */



   /* Identify which linked list should be traversed based on the protocol and           */
   /* direction.  Perform additional list management if the designated handle            */
   /* appears in the first record on the list.  If the designated list is empty          */
   /* return a value of FC_Q_NOT_FOUND.                                                  */

    if (direction < FC_RECEIVE)
    {
        if (protocol >= FC_TARGET)
        {
            if (FC_NULL == Host->TransferMgr.t_send_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.t_send_q_head->handle == handle)
            {
                record = Host->TransferMgr.t_send_q_head;
                Host->TransferMgr.t_send_q_head = record->next;
            }
            else
                locator = Host->TransferMgr.t_send_q_head;
        }
        else
        {
            if (FC_NULL == Host->TransferMgr.i_send_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.i_send_q_head->handle == handle)
            {
                record = Host->TransferMgr.i_send_q_head;
                Host->TransferMgr.i_send_q_head = record->next;
            }
            else
                locator = Host->TransferMgr.i_send_q_head;
        }
    }
    else
    {
        if (protocol >= FC_TARGET)
        {
            if (FC_NULL == Host->TransferMgr.t_receive_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.t_receive_q_head->handle == handle)
            {
                record = Host->TransferMgr.t_receive_q_head;
                Host->TransferMgr.t_receive_q_head = record->next;
            }
            else
                locator = Host->TransferMgr.t_receive_q_head;
        }
        else
        {
            if (FC_NULL == Host->TransferMgr.i_receive_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.i_receive_q_head->handle == handle)
            {
                record = Host->TransferMgr.i_receive_q_head;
                Host->TransferMgr.i_receive_q_head = record->next;
            }
            else
                locator = Host->TransferMgr.i_receive_q_head;
        }
    }

    /* Start traversing the designated linked list.  This search is looking for the      */
    /* handle to appear in the next record.  Once the handle is located:                 */
    /*    - the varable "record" is set,                                                 */
    /*    - the record is removed from the linked list,                                  */
    /*    - the contents of the record are copied out,                                   */
    /*    - the record is placed at the start of the free list                           */
    /*                                                                                   */
    /* If more than NUM_TRANSFER_RECORDS were searched for the handle, the function      */
    /* returns FC_Q_CORRUPT.  If the handle was not located, the function returns        */
    /* FC_Q_NOT_FOUND.                                                                   */


    if (FC_NULL == record)
    {
        i = 0;
        while (FC_NULL != locator->next)
        {
            if (locator->next->handle == handle)
            {
                record = locator->next;
                locator->next = record->next;
                break;
            }
            locator = locator->next;
            if (++i > NUM_TRANSFER_RECORDS)
                return (FC_Q_CORRUPT);
        }
    }

    if (FC_NULL == record)
        return (FC_Q_NOT_FOUND);

    record->next = Host->TransferMgr.free_head;
    Host->TransferMgr.free_head = record;

    Host->TransferMgr.free_count++;

    *result  =  record->TI;
    if (ReqQ_Entry)                         /* only copy if pointer is provided */
        *ReqQ_Entry = record->ReqQItem;

    return (FC_Q_SUCCESS);

}

/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE *find_q_item_by_xferID(...)
 *     Identifies which linked list to traverse to locate a transfer record with the
 *     designated transferID.  The linked list is identified by the transfer protocol and
 *     communication direction.  This routine copies the data into the result structure.
 *     Handle is also returned.
 *
 *     Special processing is performed if the record is the first element of one of the
 *     transfer linked lists.
 *
 *     If the record is located, accessed and transferred to the free list this function
 *     returns a value of FC_Q_SUCCESS.
 *
 *     If the record is not located, the linked lists are not updated and the function
 *     returns a value of FC_Q_NOT_FOUND.
 *
 *     If the search for the designated handle requires the function to traverse more than
 *     NUM_TRANSFER_RECORDS, the linked lists are not updated and the function returns a
 *     value of FC_Q_CORRUPT.
 *
 *     returns the pended Request Queue entry if present otherwise ReqQ_Entry is undefined
 **************************************************************************************/
FC_Q_ERR_TYPE find_q_item_by_xferID(HostAdapterType *Host,
                          FC_DIRECTION_TYPE direction,
                          ProtocolType protocol,
                          u32 TransferID,
                          u32 *handle,
                          TransferInfoType *result)
{
int i;  /* counter variable to keep track of the number of links traversed. */

transfer_record_type *locator = FC_NULL;  /* identifies the record prior to the one */
                                          /* containing the data                    */
transfer_record_type *record = FC_NULL;   /* points to the record containg the data */



   /* Identify which linked list should be traversed based on the protocol and           */
   /* direction.  Perform additional list management if the designated handle            */
   /* appears in the first record on the list.  If the designated list is empty          */
   /* return a value of FC_Q_NOT_FOUND.                                                  */

    if (direction < FC_RECEIVE)
    {
        if (protocol >= FC_TARGET)
        {
            if (FC_NULL == Host->TransferMgr.t_send_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.t_send_q_head->TI.TransferID == TransferID)
            {
                record = Host->TransferMgr.t_send_q_head;
            }
            else
                locator = Host->TransferMgr.t_send_q_head;
        }
        else
        {
            if (FC_NULL == Host->TransferMgr.i_send_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.i_send_q_head->TI.TransferID == TransferID)
            {
                record = Host->TransferMgr.i_send_q_head;
            }
            else
                locator = Host->TransferMgr.i_send_q_head;
        }
    }
    else
    {
        if (protocol >= FC_TARGET)
        {
            if (FC_NULL == Host->TransferMgr.t_receive_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.t_receive_q_head->TI.TransferID == TransferID)
            {
                record = Host->TransferMgr.t_receive_q_head;
            }
            else
                locator = Host->TransferMgr.t_receive_q_head;
        }
        else
        {
            if (FC_NULL == Host->TransferMgr.i_receive_q_head)
                return (FC_Q_NOT_FOUND);

            if (Host->TransferMgr.i_receive_q_head->TI.TransferID == TransferID)
            {
                record = Host->TransferMgr.i_receive_q_head;
            }
            else
                locator = Host->TransferMgr.i_receive_q_head;
        }
    }

    /* Start traversing the designated linked list.  This search is looking for the      */
    /* handle to appear in the next record.  Once the handle is located:                 */
    /*    - the varable "record" is set,                                                 */
     /*                                                                                   */
    /* If more than NUM_TRANSFER_RECORDS were searched for the handle, the function      */
    /* returns FC_Q_CORRUPT.  If the handle was not located, the function returns        */
    /* FC_Q_NOT_FOUND.                                                                   */


    if (FC_NULL == record)
    {
        i = 0;
        while (FC_NULL != locator->next)
        {
            if (locator->next->TI.TransferID == TransferID)
            {
                record = locator->next;
                break;
            }
            locator = locator->next;
            if (++i > NUM_TRANSFER_RECORDS)
                return (FC_Q_CORRUPT);
        }
    }

    if (FC_NULL == record)
        return (FC_Q_NOT_FOUND);

    *result  =  record->TI;
    *handle  =  record->handle;

    return (FC_Q_SUCCESS);

} /* find_q_item_by_xferID */

/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE *remove_q_item(...)
 *     Identifies which linked list to traverse to locate a transfer record with the
 *     designated TransferID.  The linked list is identified by the transfer protocol
 *     (always Target) and communication direction.  This routine removes the record
 *     from one of the two target transfer linked lists, activates the callback function
 *     with a status of FTS_TRANSFER_ABORTED.
 *
 *     Special processing is performed if the record is the first element of one of the
 *     transfer linked lists.
 *
 *     If the record is located, accessed and transferred to the free list this function
 *     returns a value of FC_Q_SUCCESS.
 *
 *     If the record is not located, the linked lists are not updated and the function
 *     returns a value of FC_Q_NOT_FOUND.
 *
 *     If the search for the designated handle requires the function to traverse more than
 *     NUM_TRANSFER_RECORDS, the linked lists are not updated and the function returns a
 *     value of FC_Q_CORRUPT.
 *
 **************************************************************************************/
FC_Q_ERR_TYPE remove_q_item(HostAdapterType *Host,
                          FC_DIRECTION_TYPE direction,
                          u32 TransferID)
{
int i;  /* counter variable to keep track of the number of links traversed. */

transfer_record_type *locator = FC_NULL;  /* identifies the record prior to the one */
                                          /* containing the data                    */
transfer_record_type *record = FC_NULL;   /* points to the record containg the data */



   /* Identify which linked list should be traversed based on the protocol and           */
   /* direction.  Perform additional list management if the designated handle            */
   /* appears in the first record on the list.  If the designated list is empty          */
   /* return a value of FC_Q_NOT_FOUND.                                                  */

    if (direction < FC_RECEIVE)
    {
        if (FC_NULL == Host->TransferMgr.t_send_q_head)
            return (FC_Q_NOT_FOUND);

        if (Host->TransferMgr.t_send_q_head->TI.TransferID == TransferID)
        {
            record = Host->TransferMgr.t_send_q_head;
            Host->TransferMgr.t_send_q_head = record->next;
        }
        else
            locator = Host->TransferMgr.t_send_q_head;
    }
    else
    {
        if (FC_NULL == Host->TransferMgr.t_receive_q_head)
            return (FC_Q_NOT_FOUND);

        if (Host->TransferMgr.t_receive_q_head->TI.TransferID == TransferID)
        {
            record = Host->TransferMgr.t_receive_q_head;
            Host->TransferMgr.t_receive_q_head = record->next;
        }
        else
            locator = Host->TransferMgr.t_receive_q_head;
    }

    /* Start traversing the designated linked list.  This search is looking for the      */
    /* handle to appear in the next record.  Once the handle is located:                 */
    /*    - the varable "record" is set,                                                 */
    /*    - the record is removed from the linked list,                                  */
    /*    - the contents of the record are copied out,                                   */
    /*    - the record is placed at the start of the free list                           */
    /*                                                                                   */
    /* If more than NUM_TRANSFER_RECORDS were searched for the handle, the function      */
    /* returns FC_Q_CORRUPT.  If the handle was not located, the function returns        */
    /* FC_Q_NOT_FOUND.                                                                   */


    if (FC_NULL == record)
    {
        i = 0;
        while (FC_NULL != locator->next)
        {
            if (locator->next->TI.TransferID == TransferID)
            {
                record = locator->next;
                locator->next = record->next;
                break;
            }
            locator = locator->next;
            if (++i > NUM_TRANSFER_RECORDS)
                return (FC_Q_CORRUPT);
        }
    }

    if (FC_NULL == record)
        return (FC_Q_NOT_FOUND);

    record->next = Host->TransferMgr.free_head;
    Host->TransferMgr.free_head = record;

    Host->TransferMgr.free_count++;

    record->TI.Status = FTS_ABORTED;
    if (record->TI.NotificationMethod)
        record->TI.NotificationMethod(&record->TI);

    return (FC_Q_SUCCESS);

}




/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE target_q_item(...)
 *     Identifies which linked list to traverse to locate a transfer record with the
 *     designated subaddress and initiator_id.  The linked list is identified
 *     by the transfer protocol and communication direction.  This routine copies the
 *     data from the located record into the result structure.
 *
 *     THIS FUNCTION DOES NOT ALTER THE LINKED LISTS.
 *
 *     Special processing is performed if the record is the first element of one of the
 *     transfer linked lists.
 *
 *     If the record is located and accessed this function returns a value of FC_Q_SUCCESS.
 *
 *     If the record is not located, the function returns a value of FC_Q_NOT_FOUND.
 *
 *     If the search for the designated handle requires the function to traverse more than
 *     NUM_TRANSFER_RECORDS, the function returns a value of FC_Q_CORRUPT.
 *
 **************************************************************************************/
FC_Q_ERR_TYPE target_q_item(HostAdapterType *Host,
                            FC_DIRECTION_TYPE direction,
                            u16 subaddress,
                            u8 initiator_id,
                            u32 *handle,
                            TransferInfoType **result)
{
int i;  /* counter variable to keep track of the number of links traversed. */

transfer_record_type *record;  /* Pointer to parse the linked list.     */



   /* Identify which linked list should be traversed based on the direction.             */
   /* This function only accesses the target linked lists.                               */

    if (direction < FC_RECEIVE)
        record = Host->TransferMgr.t_send_q_head;
    else
        record = Host->TransferMgr.t_receive_q_head;


    /* Search through the linked list until the designated subaddress, pre-queued status */
    /* initiator_id have been located.  Once located, transfer the data and return       */
    /* a value of FC_Q_SUCCESS.  If the record is not located, return FC_Q_NOT_FOUND.    */
    /* If more than NUM_TRANSFER_RECORDS are searched, return FC_Q_CORRUPT.              */

    i = 0;
    while (FC_NULL != record)
    {
        if ( ( record->TI.Subaddress == subaddress ) &&
             ( record->TI.Port == initiator_id ) &&
             ( record->handle & FC_ENTRY_AVAIL_MASK ) )   break;

        record = record->next;

        if (++i > NUM_TRANSFER_RECORDS)
            return (FC_Q_CORRUPT);

    }

    if (FC_NULL == record)
        return (FC_Q_NOT_FOUND);

    record->handle &= ~FC_ENTRY_AVAIL_MASK;

    *result  =  &record->TI;
    *handle  =  record->handle;

    return (FC_Q_SUCCESS);

}

/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE clear_q(HostAdapterType *Host)
 *      Traverses through each linked list, activating the notification method to inform
 *      the application that the transfer has been aborted.  After this, the element is
 *      transferred over to the free record linked list.
 *
 *      For the list traversal, the head item is removed first.  This will notify the
 *      application in request order for each particular linked list.  After the head
 *      item is removed, it is replaced by the next item on the linked list until the
 *      list is empty.
 *
 *      If more than NUM_TRANSFER_RECORDS are parsed this routine returns FC_Q_CORRUPT.
 *
 *      NOTE: the record counter "i", is not reset between linked list traverses, because
 *      the total number of records on all 4 of the linked lists can not exceed
 *      NUM_TRANSFER_RECORDS.
 *
 *
 **************************************************************************************/
FC_Q_ERR_TYPE clear_q(HostAdapterType *Host)
{
transfer_record_type *record;  /* Pointer to parse the linked lists.     */
int i = 0;                               /* record counter                         */

    /* Traverse the target send list */

    while (FC_NULL != Host->TransferMgr.t_send_q_head)
    {
        record = Host->TransferMgr.t_send_q_head;
        record->TI.Status = FTS_ABORTED;
        if (record->TI.NotificationMethod)
            record->TI.NotificationMethod(&record->TI);
        Host->TransferMgr.t_send_q_head = record->next;
        record->next = Host->TransferMgr.free_head;
        Host->TransferMgr.free_head = record;
        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }

    /* Traverse the target receive list */

    while (FC_NULL != Host->TransferMgr.t_receive_q_head)
    {
        record = Host->TransferMgr.t_receive_q_head;
        record->TI.Status = FTS_ABORTED;
        if (record->TI.NotificationMethod)
            record->TI.NotificationMethod(&record->TI);
        Host->TransferMgr.t_receive_q_head = record->next;
        record->next = Host->TransferMgr.free_head;
        Host->TransferMgr.free_head = record;
        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }

    /* Traverse the initiator send list */

    while (FC_NULL != Host->TransferMgr.i_send_q_head)
    {
        record = Host->TransferMgr.i_send_q_head;
        record->TI.Status = FTS_ABORTED;
        if (record->TI.NotificationMethod)
            record->TI.NotificationMethod(&record->TI);
        Host->TransferMgr.i_send_q_head = record->next;
        record->next = Host->TransferMgr.free_head;
        Host->TransferMgr.free_head = record;
        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }

    /* Traverse the initiator receive list */

    while (FC_NULL != Host->TransferMgr.i_receive_q_head)
    {
        record = Host->TransferMgr.i_receive_q_head;
        record->TI.Status = FTS_ABORTED;
        if (record->TI.NotificationMethod)
            record->TI.NotificationMethod(&record->TI);
        Host->TransferMgr.i_receive_q_head = record->next;
        record->next = Host->TransferMgr.free_head;
        Host->TransferMgr.free_head = record;
        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }

    return (FC_Q_SUCCESS);
}



/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE clear_q_for_port(HostAdapterType *Host, u8 PortID)
 *      Traverses through each linked list, looking for transfer records associated with the
 *      PortID.  When a record is found, the notification method is activated to inform
 *      the application that the transfer has been aborted.  After this, the element is
 *      transferred over to the free record linked list.
 *
 *      If more than NUM_TRANSFER_RECORDS are parsed this routine returns FC_Q_CORRUPT.
 *
 *      NOTE: the record counter "i", is not reset between linked list traverses, because
 *      the total number of records on all 4 of the linked lists can not exceed
 *      NUM_TRANSFER_RECORDS.
 *
 *
 **************************************************************************************/
FC_Q_ERR_TYPE clear_q_for_port(HostAdapterType *Host, u8 PortID)
{
transfer_record_type *record;            /* Pointer to parse the linked lists.     */
transfer_record_type *prev_record;       /* Pointer to the previous record to      */
                                         /* maintain list integrity and detect     */
                                         /* the special case of the first element  */
                                         /* on a list                              */
int i = 0;                               /* record counter                         */

    /* Traverse the target send list */

    record = Host->TransferMgr.t_send_q_head;
    prev_record = FC_NULL;
    while (FC_NULL != record)
    {
        if (record->TI.Port == PortID)
        {
            /* Notify the callback function that the transfer was aborted */
            record->TI.Status = FTS_ABORTED;
            if (record->TI.NotificationMethod)
                record->TI.NotificationMethod(&record->TI);

            /* perform special check if the record was at the start of the list */
            if (FC_NULL == prev_record)
            {
                Host->TransferMgr.t_send_q_head = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = Host->TransferMgr.t_send_q_head;
            }
            else
            {
                prev_record->next = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = prev_record->next;
            }
        }
        else
        {
            prev_record = record;
            record = record->next;
        }
        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }

    /* Traverse the target receive list */

    record = Host->TransferMgr.t_receive_q_head;
    prev_record = FC_NULL;
    while (FC_NULL != record)
    {
        if (record->TI.Port == PortID)
        {
            /* Notify the callback function that the transfer was aborted */
            record->TI.Status = FTS_ABORTED;
            if (record->TI.NotificationMethod)
                record->TI.NotificationMethod(&record->TI);

            /* perform special check if the record was at the start of the list */
            if (FC_NULL == prev_record)
            {
                Host->TransferMgr.t_receive_q_head = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = Host->TransferMgr.t_receive_q_head;
            }
            else
            {
                prev_record->next = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = prev_record->next;
            }
        }
        else
        {
            prev_record = record;
            record = record->next;
        }
        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }

    /* Traverse the initiator send list */

    record = Host->TransferMgr.i_send_q_head;
    prev_record = FC_NULL;
    while (FC_NULL != record)
    {
        if (record->TI.Port == PortID)
        {
            /* Notify the callback function that the transfer was aborted */
            record->TI.Status = FTS_ABORTED;
            if (record->TI.NotificationMethod)
                record->TI.NotificationMethod(&record->TI);

            /* perform special check if the record was at the start of the list */
            if (FC_NULL == prev_record)
            {
                Host->TransferMgr.i_send_q_head = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = Host->TransferMgr.i_send_q_head;
            }
            else
            {
                prev_record->next = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = prev_record->next;
            }
        }
        else
        {
            prev_record = record;
            record = record->next;
        }

        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }


    /* Traverse the initiator receive list */

    record = Host->TransferMgr.i_receive_q_head;
    prev_record = FC_NULL;
    while (FC_NULL != record)
    {
        if (record->TI.Port == PortID)
        {
            /* Notify the callback function that the transfer was aborted */
            record->TI.Status = FTS_ABORTED;
            if (record->TI.NotificationMethod)
                record->TI.NotificationMethod(&record->TI);

            /* perform special check if the record was at the start of the list */
            if (FC_NULL == prev_record)
            {
                Host->TransferMgr.i_receive_q_head = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = Host->TransferMgr.i_receive_q_head;
            }
            else
            {
                prev_record->next = record->next;
                record->next = Host->TransferMgr.free_head;
                Host->TransferMgr.free_head = record;
                record = prev_record->next;
            }
        }
        else
        {
            prev_record = record;
            record = record->next;
        }

        if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
    }

    return (FC_Q_SUCCESS);
}


/****************************************************************************************
 *
 * function FC_Q_ERR_TYPE evaluate_q(HostAdapterType *Host)
 *      Verifies that each transfer_record is contained on one of the linked lists.
 *
 *      Each link list is traversed, the pointer to a record (i.e. the record address)
 *      is used to determine the record position within the static array.  A counter is
 *      incremented for the calculated position.  At the end of the test, each static
 *      static record should have only been found once on the lists.
 *
 *      If all transfer_record elements are accounted for, the function returns FC_Q_SUCCESS.
 *
 *      If a record is located on multiple lists, or if more than NUM_TRANSFER_RECORDS
 *      are detected, the function returns a FC_Q_CORRUPT.
 *
 *
 *      NOTE: the record counter "i", is not reset between linked list traverses, because
 *      the total number of records on all 5 of the linked lists can not exceed
 *      NUM_TRANSFER_RECORDS.
 *
 *
 **************************************************************************************/
FC_Q_ERR_TYPE evaluate_q(HostAdapterType *Host)
{
u32 *record_count = Fibre_Malloc(NUM_TRANSFER_RECORDS * sizeof(u32));     /* counts the number of times a particular record   */
                                            /* was located on the linked lists                  */
int i;                                      /* loop index counter                               */
transfer_record_type  *record;              /* pointer to traverse linked lists                 */
u32 record_size;                            /* the size of a transfer record                    */
u32 record_index;                           /* maps a linked list record to its static array    */
FC_Q_ERR_TYPE Status = FC_Q_SUCCESS;

    /* clear the record count array */

    for (i = 0; i < NUM_TRANSFER_RECORDS; i++)  record_count[i] = 0;

    /* define the record size */

    record_size = sizeof(transfer_record_type);

    /* traverse each linked list.  For each record on the linked list, calculate the static   */
    /* array offset from the linked list pointer, the linked list record size and             */
    /* the address of the first record in the static array.  If the static array offset       */
    /* (record_index) exceeds the static array size, return FC_Q_CORRUPT.  If more than       */
    /* NUM_TRANSFER_RECORDS are traversed on the 5 linked lists, return FC_Q_CORRUPT.         */


    i = 0;

    if (FC_NULL != Host->TransferMgr.i_receive_q_head)
    {
        record = Host->TransferMgr.i_receive_q_head;
        for ( ; ; )
        {
            record_index = (u32)((char*)record - (char*)Host->TransferRecord)/record_size;
            if (record_index >= NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
            record_count[record_index]++;

            record = record->next;

            if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);

            if (FC_NULL == record) break;

        }
    }

    if (FC_NULL != Host->TransferMgr.t_receive_q_head)
    {
        record = Host->TransferMgr.t_receive_q_head;
        for ( ; ; )
        {
            record_index = (u32)((char*)record - (char*)Host->TransferRecord)/record_size;
            if (record_index >= NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
            record_count[record_index]++;

            record = record->next;

            if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);

            if (FC_NULL == record) break;

        }
    }

    if (FC_NULL != Host->TransferMgr.i_send_q_head)
    {
        record = Host->TransferMgr.i_send_q_head;
        for ( ; ; )
        {
            record_index = (u32)((char*)record - (char*)Host->TransferRecord)/record_size;
            if (record_index >= NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
            record_count[record_index]++;

            record = record->next;

            if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);

            if (FC_NULL == record) break;

        }
    }

    if (FC_NULL != Host->TransferMgr.t_send_q_head)
    {
        record = Host->TransferMgr.t_send_q_head;
        for ( ; ; )
        {
            record_index = (u32)((char*)record - (char*)Host->TransferRecord)/record_size;
            if (record_index >= NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
            record_count[record_index]++;

            record = record->next;

            if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);

            if (FC_NULL == record) break;

        }
    }

    if (FC_NULL != Host->TransferMgr.free_head)
    {
        record = Host->TransferMgr.free_head;
        for ( ; ; )
        {
            record_index = (u32)((char*)record - (char*)Host->TransferRecord)/record_size;
            if (record_index >= NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);
            record_count[record_index]++;

            record = record->next;

            if (++i > NUM_TRANSFER_RECORDS) return(FC_Q_CORRUPT);

            if (FC_NULL == record) break;

        }
    }

    for (i = 0; i < NUM_TRANSFER_RECORDS; i++)
    {
        if (1 != record_count[i])
            Status = FC_Q_CORRUPT;
    }

    Fibre_Free(record_count,NUM_TRANSFER_RECORDS * sizeof(u32));
    return (Status);

}
