-------------------------------------------------------------------------------
--
-- Interrupts
--
-- CSCI:                  Simulation, Training and Analysis System
-- CSC:                   Common
--
-- Author: Stanley W. Bray
--
-- Creation Date:          29-Jul-05
--
-- Revision History:
--
--    Revision  PR/CR                 Author                 Date
--
--    1.0       PR xxxx               Stanley W. Bray        29-Jul-05
--       Initial Delivery
--
-- Description:
--    This package provides the types necessary to decribe all the interrupts
--    possible.
--
-- Exceptions Propagated:
--    None
--
-------------------------------------------------------------------------------
--
package Interrupts is

   type Interrupt_Name is
     (ACM1_Card_1_Channel_A_Encoder,
      ACM1_Card_1_Channel_B_Encoder,
      ACM1_Card_1_Channel_C_Encoder,
      ACM1_Card_1_Channel_D_Encoder,
      ACM1_Card_1_Channel_E_Encoder,
      ACM1_Card_1_Channel_F_Encoder,
      ACM1_Card_2_Channel_A_Encoder,
      ACM1_Card_2_Channel_B_Encoder,
      ACM1_Card_2_Channel_C_Encoder,
      ACM1_Card_2_Channel_D_Encoder,
      ACM1_Card_2_Channel_E_Encoder,
      ACM1_Card_3_Channel_A_Encoder,
      ACM1_Card_3_Channel_B_Encoder,
      ACM1_Card_3_Channel_C_Encoder,
      ACM1_Card_3_Channel_D_Encoder,
      ACM1_Card_3_Channel_E_Encoder,
      ACM1_Card_4_Channel_A_Encoder,
      ACM1_Card_4_Channel_B_Encoder,
      ACM1_Card_4_Channel_C_Encoder,
      ACM1_Card_4_Channel_D_Encoder,
      ACM1_Card_4_Channel_E_Encoder,
      ACM1_Card_1_Error_CH_A,
      ACM1_Card_1_Error_CH_B,
      ACM1_Card_1_Error_CH_C,
      ACM1_Card_1_Error_CH_D,
      ACM1_Card_1_Error_CH_E,
      ACM1_Card_1_Error_CH_F,
      ACM1_Card_2_Error_CH_A,
      ACM1_Card_2_Error_CH_B,
      ACM1_Card_2_Error_CH_C,
      ACM1_Card_2_Error_CH_D,
      ACM1_Card_2_Error_CH_E,
      ACM1_Card_3_Error_CH_A,
      ACM1_Card_3_Error_CH_B,
      ACM1_Card_3_Error_CH_C,
      ACM1_Card_3_Error_CH_D,
      ACM1_Card_3_Error_CH_E,
      ACM1_Card_4_Error_CH_A,
      ACM1_Card_4_Error_CH_B,
      ACM1_Card_4_Error_CH_C,
      ACM1_Card_4_Error_CH_D,
      ACM1_Card_4_Error_CH_E,
      DDS,
      DDS_Error,
      DDS_DSR,
      Serial_IO,
      Upper_Flat_Panel,
      Upper_Flat_Panel_Error,
      Upper_Flat_Panel_DSR,
      Lower_Flat_Panel,
      Lower_Flat_Panel_Error,
      Lower_Flat_Panel_DSR,
      Interprocessor,
      Interprocessor_Error,
      Interprocessor_DSR,
      PIO2_Card_1_Vector_0_7,
      PIO2_Card_1_Vector_8_15,
      PIO2_Card_1_Vector_16_23,
      PIO2_Card_1_Vector_24_31,
      PIO2_Card_1_Spurious,
      PIO2_Card_2_Spurious,
      Counter_1,
      Counter_2,
      Counter_3,
      Counter_4,
      Counter_5,
      Counter_6,
      PIO2_Card_2_Vector_0_7,
      PIO2_Card_2_Vector_8_15,
      PIO2_Card_2_Vector_16_23,
      PIO2_Card_2_Vector_24_31,
      Counter_1_Card_2,
      Counter_2_Card_2,
      Counter_3_Card_2,
      Counter_4_Card_2,
      Counter_5_Card_2,
      Counter_6_Card_2,
      Heartbeat
     );

   for Interrupt_Name'size use 8;

   subtype ACM1_Interrupt_Name is Interrupt_Name range
     ACM1_Card_1_Channel_A_Encoder .. ACM1_Card_4_Error_CH_E;

   subtype DDS_Interrupt_Name is Interrupt_Name range
     DDS .. DDS_DSR;

   subtype Flat_Panel_Interrupt_Name is Interrupt_Name range
     Upper_Flat_Panel .. Lower_Flat_Panel_DSR;

   subtype Interprocessor_Interrupt_Name is Interrupt_Name range
     Interprocessor .. Interprocessor_DSR;

   subtype PIO_Interrupt_Name is Interrupt_Name range
     PIO2_Card_1_Vector_0_7 .. Counter_6_Card_2;

end Interrupts;
