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
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Exceptions; use Ada.Exceptions;

package body SOCKETS is

   ----------------------------------------------------------------------------
   --
   -- Open_Socket
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Open_Socket ( Port_Num : in Port_Type; Local_Record : in out Socket_Record ) is
      Address : Sock_Addr_Type;
      Server : Socket_Type;
      Socket : Socket_Type;
      Channel : Stream_Access;
      Socket_Record : Port_Num_Record;
   begin
      Address.Addr := Addresses (Get_Host_By_Name(Host_Name), 1);
      Address.Port := Port_Num;
      Create_Socket (Server);

      Set_Socket_Option
        (Server,
         Socket_Level,
         (Reuse_Address, True));

      Bind_Socket (Server, Address);
      Listen_Socket (Server);
      Accept_Socket (Server, Socket, Address);
      Channel := Stream (Socket);

      delay 0.2;

      -- Store local record
      Local_Record.Port := Port_Num;
      Local_Record.Channel := Channel;
      Local_Record.Server := Server;
      Local_Record.Socket := Socket;

   end Open_Socket;


   ----------------------------------------------------------------------------
   --
   -- Close_Socket
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Close_Socket ( Local_Record : in out Socket_Record ) is
   begin
      Close_Socket( Local_Record.Server );
      Close_Socket( Local_Record.Socket );
   end Close_Socket;


   ----------------------------------------------------------------------------
   --
   -- Send_Data_Task
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   task body Send_Data_Task is
   begin
      loop
         begin
            accept Write ( Message : String;  Local_Record : Socket_Record ) do
               String'Output ( Local_Record.Channel, Message );
            end;

         exception
            when others =>
               null;
         end;
      end loop;
   end Send_Data_Task;


   ----------------------------------------------------------------------------
   --
   -- Receive_Data
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Receive_Data (Local_Record : in Socket_Record)  return String is
      Current_Record : Socket_Record := Local_Record;
   begin

      return String'Input ( Local_Record.Channel );

   exception
      when others =>
         Close_Socket ( Current_Record );
         return "";

   end Receive_Data;

end SOCKETS;


