/*********************************************************************************************

    COPYRIGHT CRITICAL I/O, LLC.  ALL RIGHTS RESERVED

    FILE NAME :             FC_Q_TYP.H    $Revision: 1.1 $
    Module Description:     Transfer information queue data types


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
    11/09/02    Version 4.0 See rel4_0.txt
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

#ifndef FC_Q_TYP_H
#define FC_Q_TYP_H



/* Queue Monitor Resource Manager
 *
 *  Data transfers from targets must be queued up by the FCRM.  When the FCRM
 *  receives a transfer request from an initiator, the queue is searched to
 *  locate the desired data buffer.  The Queue Monitor consists of two key data
 *  objects:
 *       The Resource Manager - target_queue_mgr_type
 *       Target Transfer Definitions - an array of target_transfer_type
 *
 *  The Target Transfer Definitions is a statically allocated array of elements.
 *  These elements are used to form linked lists.   The array will identify
 *  Target receive transfers and Target send transfers.
 *
 *  The Resource Manager will provide a pointer to the head queue elements of the
 *  send, receive and free linked lists. If the free pointer is NULL, no more records
 *  are available.
 *
 *  The Target Transfer Definitions will be initialized to place all entries on the
 *  free list.  The Resource Manager will be initialized to have the send and receive
 *  pointers set to NULL and the free pointer designating the first element (which will
 *  be linked to the second, then third, etc.).
 *
 * */





/* Error codes */
typedef enum
{
    FC_Q_SUCCESS,  /* 0 */
    FC_Q_FULL,     /* The queue is full, MAX_CONCURRENT_COMMANDS records exist */
    FC_Q_CORRUPT,  /* The queue is corrupted, searched through more than MAX_CONCURRENT_COMMANDS */
    FC_Q_NOT_FOUND /* The element was not located */
}FC_Q_ERR_TYPE;



/* Transfer Element Data Structure  */


struct transfer_record
{
    TransferInfoType TI;
    tCTIOType2IOCB ReqQItem;      /* added for pended target I/O support */
    u32 handle;
    struct  transfer_record *next;
};

typedef struct transfer_record transfer_record_type;




/* Resource Manager Data Structure */

typedef struct
{
    transfer_record_type *t_send_q_head;     /* the first element in the target send linked list */
    transfer_record_type *t_receive_q_head;  /* the first element in the target receive linked list */
    transfer_record_type *i_send_q_head;     /* the first element in the initiator send linked list */
    transfer_record_type *i_receive_q_head;  /* the first element in the initiator receive linked list */
    transfer_record_type *free_head;         /* the first element in the free linked list */
    u32                   free_count;        /* the number of free records left */
}transfer_queue_mgr_type;



#endif
