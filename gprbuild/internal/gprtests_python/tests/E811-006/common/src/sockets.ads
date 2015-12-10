-------------------------------------------------------------------------------
--
-- SOCKETS
--
-- CSCI:                  Simulation, Training and Analysys System
-- CSC:                   Common
--
-- Description:
--    This package sets up the TCP connections.
--
-- Exceptions Propagated:
--
-------------------------------------------------------------------------------
--
with GNAT.Sockets;  use GNAT.Sockets;

package SOCKETS is

   -- Record that stores the predefined port number
   type Port_Num_Record is
      record
         PORT_Upper_FP : Port_Type := 50000;
         PORT_Lower_FP : Port_Type := 50001;
         PORT_IODM : Port_Type := 50010;
         STBD_Upper_FP : Port_Type := 50020;
         STBD_Lower_FP : Port_Type := 50021;
         STBD_IODM : Port_Type := 50030;
      end record;

   -- Record that stores the socket data shared between send/receive tasks
   type Socket_Record is
      record
         Port : Port_Type;
         Channel : Stream_Access;
         Server : Socket_Type;
         Socket : Socket_Type;
      end record;

   -- The procedure Open_Socket opens a connection at Port_Num.
   procedure Open_Socket ( Port_Num : in Port_Type; Local_Record : in out Socket_Record );

   -- The procedure Close_Socket closes the TCP connection.
   procedure Close_Socket ( Local_Record : in out Socket_Record );

   -- The task Send_Data_Task sends messages to the a socket.
   task type Send_Data_Task is
      entry Write ( Message : String;  Local_Record : Socket_Record );
   end Send_Data_Task;

   -- The procedure Receive_Data performs a TCP receive operation.
   function Receive_Data ( Local_Record : in Socket_Record ) return String;

end SOCKETS;

