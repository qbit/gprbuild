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
-- Program      : EA-18 FOSJ
--
-- CSCI         : ASC
--
-- DATE CREATED : 12/18/2004
--
-- AUTHOR       : G. Prete
--
-- PURPOSE      : This package will contain System common data types used by
--                all address spaces.
--
--
-- MODIFICATION HISTORY
--
--     AUTHOR        :  R. Sinclair and John DeBois
--     DATE          :  May 03, 2005
--     MODIFICATION  :  Added T_UTC_Time (With Interfaces.C)
--
--     AUTHOR        :  D. Lane
--     DATE          :  May 26, 2005
--     MODIFICATION  :  Added types used in the HPI to SA/RM to POD interfaces
--
--     AUTHOR        :  A. Page
--     DATE          :  June 14, 2005
--     MODIFICATION  :  Added subtypes for the MAL Header used in the HPI to 
--                      respond to taskfile commands.
--
--     Author        :  J. Bates
--     Date          :  July 5, 2005
--     Modification  :  Removed the following types from this file in order to
--                   :  move towards the "Common_Types":
--                       T_Source_Indicators
--                       T_Transmitter_Source_1_2_3_4_Disabled
--                       T_Transmitter_Source_1_2_3_4_Locked
--                       T_Transmitter_Source_1_2_3_4_Failure
--                       T_UEU_Mode
--                       T_Jam_Steering
--                       T_Assignment_Requirement_Id
--                       T_Made_or_Not_Made_Reason
-- 
--     Author        :  J. Bates
--     Date          :  July 12, 2005
--     Modification  :  Removed the following types to continue to move towards
--                      "Common_Types"
--                      T_Channel_Id 
--                      T_Source_Id 
--                      T_Installed_Weapon_Station 
--
--     Author        :  Sager
--     Date          :  October 15, 2006
--     Modification  :  Added type T_MDB_Load_Status for Operational Status
--                      messages.
--
--     Author        :  Sager
--     Date          :  October 23, 2006
--     Modification  :  Added type T_Blanking_Priority_Type for Jammer 
--                      Management Status messages.
--
--     AUTHOR        :  G. Prete
--     DATE          :  12-9-06
--     MODIFICATION  :  SPR 11060
--                      Increase Connection Hog Threshold to 1.5 msec
--
--                      
--     AUTHOR        :  G. Prete
--     DATE          :  1-04-07
--     MODIFICATION  :  SPR 6266
--                      IP Logger usability improvements
--                      
-- ------------------------------------------------------------------------ --
-- ------------------------------------------------------------------------ --
With Interfaces.C;
With Common_Types;

Package System_Common_Types is

     
   -- --------------------------------------------------------------------- --
   -- --------------------------------------------------------------------- --

   --
   --  When instantiating a connection, this enumeration specifies
   --  which types of communications will be supported on it.
   --  Setting the connection direction type to "SEND_ONLY" will
   --  prevent the creation of an "Input Task" for that connection.
   --
   Type Connection_Direction_Type is
      ( SEND_ONLY,
        RECEIVE_ONLY,
        BI_DIRECTIONAL );


   Type OFP_TYPE is
      ( Bootloader,
        Classified,
        Unclassified );


   Type Byte is range 0..255;
   For Byte'Size use 8;

      
   --
   --  CPU Hog threshold values for Generics.
   --
   Subtype Connection_Threshold_Type  is integer range 1..5000;
   Subtype Synch_Task_Threshold_Type  is integer range 1..5000;
   Subtype Asynch_Task_Threshold_Type is integer range 1..5000;   

      
   Type T_UTC_Time is
   Record
      Time_And_Day_Valid : Boolean;
      UTC_Year_Valid     : Boolean;
      UTC_Hours          : Interfaces.C.Unsigned;
      UTC_Minutes        : Interfaces.C.Unsigned;
      UTC_Seconds        : Interfaces.C.Unsigned;
      UTC_Microseconds   : Interfaces.C.Unsigned;
      UTC_Day_Of_Year    : Interfaces.C.Unsigned;
      UTC_Year           : Interfaces.C.Unsigned;
      UTC_Time_FOM       : Interfaces.C.Unsigned;
   End Record;


   --
   --  AMC Time is a long float - units are SECONDS
   --
   subtype T_AMC_Time is Long_Float;

   --Pod LBT/RF Prep status
   type T_Prep_Status is (
      Complete,
      Failed,
      Not_Applicable); 

      
   -- To be removed.
   -- used in Pod_Jammer_Assign_Data.adb
   type T_Requirement_Type  is
         ( E_SITE,     -- Operator assignment to a site/system
           E_PA,       -- Operator PA Library
           E_AA,       -- Operator Alarm Assignment to emiitter 
                       -- (may include MATT)
           E_RA,       -- Automatic Reactive Assignment to an emitter
           E_Phase_PA, -- Phase Preemptive Assignment to an emitter
           E_JATO      -- JATO Assignment - how ever this works
          );

   
   -- 6/20/05 R. Sinclair - Starting to move the MAL status information
   --                       to system global.
   Type T_Status is 
      ( Pending                 ,
        In_Work                 ,
        Completed               ,
        Stopped                 ,
        Failed                  ,
        Completed_With_Failures ,
        Done_Recording          , 
        Writing                 );

   
   Type T_Failure_Reason is 
      ( Success                       ,
        Invalid_Filename              , 
        Unauthorized_Subsystem        ,
        EOF_Error                     , 
        Data_Encryption_Error         , 
        Other_Error                   ,
        Door_Open                     ,
        BIT_Active                    ,
        Erase_Active                  ,
        PC_Card_Not_Installed         ,
        PC_Card_Full                  ,
        Invalid_Mode                  ,
        Bulk_Memory_Write_Successful  ,
        FAF_Update_Successful         ,
        Secure_Delete_Failed          ,
        DMD_Offline                   ,
        Subsystem_Communication_Error ,
        OFP_Load_Error                ,
        EAU_Audio_Error               ,
        CCS_Audio_Error               ,
        Other_Audio_Error             );
   
   
   --These are subtypes to reference the HPI_Common.T_HPI_MAL_Status_Header
   --it is being moved here at some point in the future; however, to permit  
   --us to press ahead I have done this as an interim step. ABPage
--   subtype Id        is HPI_Common_Types.T_Taskfile_Type;
--   subtype Tf_Status is HPI_Common_Types.T_Status;
   type T_MAL_Status_Header is
   record
      Id_Type                  : Common_Types.T_Unsigned_32;
      Id_Number                : Common_Types.T_Unsigned_32;
      Status                   : T_Status;
   end record; 
   
   Null_MAL_Status_Header : constant T_MAL_Status_Header :=
   ( Id_Type                  => 0,
     Id_Number                => 0,
     Status                   => Pending
   );                   
   
   Type T_Standard_MAL_Ack_Msg is                     
   Record                                        
      MAL_Status_Header : T_MAL_Status_Header;
   End Record;
   
   Type T_Standard_MAL_Ack_Msg_Access is access T_Standard_MAL_Ack_Msg;
   
   
   Type T_Boolean_Array_4 is array (1..4) of boolean;
   
--------------------------------------------------------------------------   
--------------------------------------------------------------------------
   type T_split_MAL_Status_Header is
   record
      Id_Type                  : Common_Types.T_Unsigned_32;
      Id_Number                : Common_Types.T_Unsigned_32;
      Status                   : T_Status;
      HPI_Split_Request_ID_Num : Integer;
   end record; 
   
   Null_split_MAL_Status_Header : constant T_split_MAL_Status_Header :=
   ( Id_Type                  => 0,
     Id_Number                => 0,
     HPI_Split_Request_ID_Num => 0,
     Status                   => Pending  
   );                   
   
   Type T_split_Standard_MAL_Ack_Msg is                     
   Record                                        
      split_MAL_Status_Header : T_split_MAL_Status_Header;
   End Record;
   
   Type T_split_Standard_MAL_Ack_Msg_Access is 
   access T_split_Standard_MAL_Ack_Msg;
   

   --------------------------------------------------------------------------
   --------------------------------------------------------------------------

   -- Build 2 Interval 1
   Type T_System_Mode is ( Standby ,
                           Receive ,
                           Radiate );

   Type T_Jamming_Mode is ( Manual    ,
                            Automatic );

   Type T_Analysis_Mode is ( None                                      ,
                             Dwell_and_Analysis_Lookthroughs           ,
                             Regularly_Scheduled_Analysis_Lookthroughs );

   Type Mode_Type is
      ( Off_Uninitialized,
        Powerup_Initializing,
        Mission_Operational,
        Loading_Libraries,
        Mission_Capable,
        Failed
      );
   
   Type MATT_Classified_Type is
      ( Unknown,
        Classified,
        Declassify_In_Progress
      ); 

   Type Receiver_Type is array(1..4) of boolean;
   
   type T_Training_Mode_On_Off is (E_On, E_Off);

    
   -- Done in Build 2 Interval 3
                       
   Type T_MDB_Load_Status is
      ( Not_Attempted,
        Load_In_Progress,
        Load_Completed
      );
      
   Type T_Blanking_Priority is
      ( ALQ_218_Blanking_Priority,
        CCS_Blanking_Priority
      );
      
   Type T_Blanking_CSC is
      ( ALQ_218,
        CCS
      );
   
  
End System_Common_Types;

