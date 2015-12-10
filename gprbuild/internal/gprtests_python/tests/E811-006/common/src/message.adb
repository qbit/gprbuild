-------------------------------------------------------------------------------
--
-- Message
--
-- CSCI:                  Simulation, Training and Analysis System
-- CSC:                   Common
--
-- Author: Stanley W. Bray
--
-- Creation Date:          21-Jul-05
--
-- Revision History:
--
--    Revision  PR/CR                 Author                 Date
--
--    1.0       PR xxxx               Stanley W. Bray        21-Jul-05
--       Initial Delivery
--
-- See Package Spec for additional information.
-------------------------------------------------------------------------------
--
with Interfaces.C; use Interfaces.C;
with System;
package body Message is

   OK : constant := 0;

   Data_Size
     : constant := Interrupts.Interrupt_Name'Size / System.Storage_Unit;

   ----------------------------------------------------------------------------
   --
   -- MessageCreate
   --
   -- Description:
   --    This procedure refers to a C function that creates the message
   --    queue and initializes the message queue object.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MessageCreate ( Key : in     Char_Array ) return Object;

   pragma Import ( C, MessageCreate, "messageCreate" );


   ----------------------------------------------------------------------------
   --
   -- Create
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Create ( Name : String ) return Object is

      Id : Object;

   begin

      Id := MessageCreate (Key => To_C (Name));

      if Id = null then

         raise Error;

      end if;

      return Id;

   end;


   ----------------------------------------------------------------------------
   --
   -- MessageSend
   --
   -- Description:
   --    This procedure refers to a C function that sends the selected message
   --    to the message queue.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MessageSend
     (Id   : in     Object;
      Data : in     System.Address;
      Size : in     Integer
     ) return Integer;

   pragma Import ( C, MessageSend, "messageSend" );


   ----------------------------------------------------------------------------
   --
   -- Send
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Send
     (Id   : in     Object;
      Data : in     Interrupts.Interrupt_Name
     ) is

   begin

      if MessageSend (Id => Id, Data => Data'address, Size => Data_Size ) /= OK then

         raise Error;

      end if;
   end;


   ----------------------------------------------------------------------------
   --
   -- MessageRecv
   --
   -- Description:
   --    This procedure refers to a C function that receives a message from
   --    the message queue.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MessageRecv
     (Id          : in     Object;
      Data_Access : in     System.Address;
      Size        : in     Integer;
      Wait        : in     Boolean
     ) return Integer;

   pragma Import ( C, MessageRecv, "messageRecv" );


   ----------------------------------------------------------------------------
   --
   -- Receive
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Receive (Id : in     Object) return Interrupts.Interrupt_Name is

      Message_Id :
        Interrupts.Interrupt_Name := Interrupts.Interrupt_Name'First;

   begin

      if MessageRecv
        (Id          => Id,
         Data_Access => Message_Id'address,
         Size        => Data_Size,
         Wait        => True
        ) /= OK then

         raise Error;

      end if;

      return Message_Id;

   end;


   ----------------------------------------------------------------------------
   --
   -- MessageFree
   --
   -- Description:
   --    This procedure refers to a C function that deletes the message queue.
   --
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MessageFree (Id   : in     Object) return Integer;

   pragma Import ( C, MessageFree, "messageFree" );


   ----------------------------------------------------------------------------
   --
   -- Free
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Free (Id : in     Object) is
   begin

      if MessageFree ( Id => Id ) /= OK then

         raise Error;

      end if;

   end;


   ----------------------------------------------------------------------------
   --
   -- MessageDelete
   --
   -- Description:
   --    This procedure refers to a C function that deletes the message queue
   --    if it exists and frees the memory allocated for the object.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MessageDelete (Id   : in     Object) return Integer;

   pragma Import ( C, MessageDelete, "messageDelete" );


   ----------------------------------------------------------------------------
   --
   -- Delete
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Delete (Id : in     Object) is
   begin

      if MessageDelete (Id => Id) /= OK then

         raise Error;

      end if;

   end;


   ----------------------------------------------------------------------------
   --
   -- MessageCreated
   --
   -- Description:
   --    This function refers to a C function that returns true if the
   --    message queue has been created.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MessageCreated (Id   : in     Object) return Boolean;

   pragma Import ( C, MessageCreated, "messageCreated" );


   ----------------------------------------------------------------------------
   --
   -- Is_Created
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Is_Created (Id : Object) return Boolean is
   begin
      return MessageCreated (Id => Id);
   end;

end Message;
