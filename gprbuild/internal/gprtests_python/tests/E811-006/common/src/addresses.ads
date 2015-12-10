-------------------------------------------------------------------------------
--
-- Addresses
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
-- Description:
--    This package provides for memory mapping of VME addresses to shared
--    memory to simulate the VME memory map.
--
-- Exceptions Propagated:
--    Storage_Error
--
-------------------------------------------------------------------------------
--

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Unchecked_Conversion;

package Addresses is

   pragma Elaborate_Body;

   type Size_Type is ( Small, Medium, Large );
   for Size_Type use ( Small => 0, Medium => 1, Large => 2 );

   type Processor is ( Port, Starboard );
   for Processor use ( Port => 0, Starboard => 1 );

   ----------------------------------------------------------------------------
   --
   -- Initialize
   --
   -- Description:
   --    This procedure initializes the shared memory segments and optionally
   --    clears the memory to zero.  This procedure should be called before
   --    any access to simulated VME memory is made.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   procedure Initialize (Clear : in     Boolean);

   ----------------------------------------------------------------------------
   --
   -- Set_Processor
   --
   -- Description:
   --    This procedure sets the default processor so that it does not need
   --    to be specified in the Get calls.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   procedure Set_Processor (Id : in     Processor);

   ----------------------------------------------------------------------------
   --
   -- Get_Processor
   --
   -- Description:
   --    This function returns the current processor.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function Get_Processor return Processor;

   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- Description:
   --    This function returns the shared memory address for the VME address
   --    represented by Value.  Size indicates the shared memory size segment
   --    to be created. The default processor is used.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (Value  : in     Integer_Address;
      Size   : in     Size_Type := Medium
     ) return System.Address;


   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- Description:
   --    This function returns the shared memory address for the VME address
   --    represented by Value.  Size indicates the shared memory size segment
   --    to be created. The default processor is used.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (Value  : in     System.Address;
      Size   : in     Size_Type := Medium
     ) return System.Address;


   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- Description:
   --    This function returns the shared memory address for the VME address
   --    represented by Value.  Size indicates the shared memory size segment
   --    to be created. Processor indicates which processor's VME memory
   --    to use.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (
      Value : in     Integer_Address;
      Size  : in     Size_Type := Medium;
      Id    : in     Processor
     )
      return System.Address;

   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- Description:
   --    This function returns the shared memory address for the VME address
   --    represented by Value.  Size indicates the shared memory size segment
   --    to be created. Processor indicates which processor's VME memory
   --    to use.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (
      Value : in     System.Address;
      Size  : in     Size_Type := Medium;
      Id    : in     Processor
     )
      return System.Address;

   ----------------------------------------------------------------------------
   --
   -- Rev
   --
   -- Description:
   --    This function returns the integer address of the VME address
   --    represented by the shared memory parameter Address.  The default
   --    processor memory is used.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function Rev (Address : in     System.Address) return Integer_Address;

   ----------------------------------------------------------------------------
   --
   -- Free
   --
   -- Description:
   --    This procedure frees all shared memory segments.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   procedure Free;

   ----------------------------------------------------------------------------
   --
   -- To_Access
   --
   -- Description:
   --    This function returns the access type of the supplied generic
   --    parameter when supplied a System Address.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   generic

      type Item_Type is private;

      type Item_Type_Access is access Item_Type;

   function To_Access (Source : in     System.Address) return Item_Type_Access;

   ----------------------------------------------------------------------------
   --
   -- To_Addr
   --
   -- Description:
   --    This function returns an unchecked conversion of a System Address
   --    when supplied an Integer Address.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function To_Addr is new Unchecked_Conversion
     (Integer_Address, System.Address);

   ----------------------------------------------------------------------------
   --
   -- To_Int_Addr
   --
   -- Description:
   --    This function returns an unchecked conversion of an Integer_Address
   --    when supplied a System Address.
   --
   -- Exceptions Propagated:
   --    Storage_Error
   --
   ----------------------------------------------------------------------------
   --
   function To_Int_Addr is new Unchecked_Conversion
     ( Source => System.Address, Target => Integer_Address);

   ----------------------------------------------------------------------------
   --
   -- Set_Path
   --
   -- Description:
   --    This procedure sets the path to the data area.
   --
   -- Exceptions Propagated:
   --    None
   --
   ----------------------------------------------------------------------------
   --
   procedure Set_Path (Path : in     String);

   ----------------------------------------------------------------------------
   --
   -- Get_Path
   --
   -- Description:
   --    This function returns the path to the data area.
   --
   -- Exceptions Propagated:
   --    None
   --
   ----------------------------------------------------------------------------
   --
   function Get_Path return String;

end Addresses;
