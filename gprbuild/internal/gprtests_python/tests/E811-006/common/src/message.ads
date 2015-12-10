-------------------------------------------------------------------------------
--
-- Message
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
--    This package provides for operating system message queues.
--
-- Exceptions Propagated:
--    Storage_Error
--
-------------------------------------------------------------------------------
--
with Interrupts;
package Message is

   type Object is private;

   Error : Exception;

   ----------------------------------------------------------------------------
   --
   -- Create
   --
   -- Description:
   --    This function creates an object for a message queue and returns the
   --    object.
   --
   -- Exceptions Propagated:
   --    Error
   --
   ----------------------------------------------------------------------------
   --
   function Create( Name : String ) return Object;

   ----------------------------------------------------------------------------
   --
   -- Send
   --
   -- Description:
   --    This procedure sends an interrupt ID to the message queue.
   --
   -- Exceptions Propagated:
   --    Error
   --
   ----------------------------------------------------------------------------
   procedure Send
     (Id   : in     Object;
      Data : in     Interrupts.Interrupt_Name
     );

   ----------------------------------------------------------------------------
   --
   -- Receive
   --
   -- Description:
   --    This function blocks until a message is received on the message
   --    queue and then returns it.
   --
   -- Exceptions Propagated:
   --    Error
   --
   ----------------------------------------------------------------------------
   function Receive (Id : in     Object) return Interrupts.Interrupt_Name;

   ----------------------------------------------------------------------------
   --
   -- Free
   --
   -- Description:
   --    This procedure deletes the message queue from the operating system.
   --
   -- Exceptions Propagated:
   --    Error
   --
   ----------------------------------------------------------------------------
   procedure Free (Id : in     Object);

   ----------------------------------------------------------------------------
   --
   -- Delete
   --
   -- Description:
   --    This procedure deletes the message queue from the operating system if
   --    it exists and frees the memory used by the object.
   --
   -- Exceptions Propagated:
   --    Error
   --
   ----------------------------------------------------------------------------
   procedure Delete (Id : in     Object);

   ----------------------------------------------------------------------------
   --
   -- Is_Created
   --
   -- Description:
   --    This function returns true if the message queue has been created.
   --
   -- Exceptions Propagated:
   --    Error
   --
   ----------------------------------------------------------------------------
   function Is_Created (Id : Object) return Boolean;

   private

      -- The real object points to a C structure
      type Object is access Integer;

end Message;
