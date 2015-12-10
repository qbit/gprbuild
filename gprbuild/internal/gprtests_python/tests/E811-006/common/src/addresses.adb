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
-- See Package Spec for additional information.
-------------------------------------------------------------------------------
--
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
package body Addresses is

   Error : constant := -1;

   Processor_Id : Processor := Port;

   ----------------------------------------------------------------------------
   --
   -- Set_Processor
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Set_Processor (Id : Processor) is
   begin
      Processor_Id := Id;
   end;

   ----------------------------------------------------------------------------
   --
   -- Get_Processor
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Get_Processor return Processor is
   begin
      return Processor_Id;
   end;

   ----------------------------------------------------------------------------
   --
   -- MemInit
   --
   -- Description:
   --    This function refers to a C function that intializes the shared
   --    memory segments and optionally clears the shared memory to zero.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MemInit (Clear : in     Boolean) return Integer;
   pragma Import ( C, MemInit, "memInit" );


   ----------------------------------------------------------------------------
   --
   -- MemMap
   --
   -- Description:
   --    This function refers to a C function that creates a shared memory
   --    segment of the specified size and for the specified processor
   --    if necessary and maps the supplied address to the shared memory
   --    address.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MemMap
     (
      Address : in     Integer_Address;
      Size    : in     Size_Type;
      Id      : in     Processor
     )
      return System.Address;

   pragma Import ( C, MemMap, "memMap" );


   ----------------------------------------------------------------------------
   --
   -- MemRevMap
   --
   -- Description:
   --    This function refers to a C function that looks up the shared memory
   --    address and returns the corresponding VME address.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MemRevMap (Address : in     System.Address) return Integer_Address;
   pragma Import ( C, MemRevMap, "memRevMap" );


   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (
      Value  : in     Integer_Address;
      Size   : in     Size_Type := Medium
     )
      return System.Address is
   begin

      return Get (Value => Value, Size => Size, Id => Processor_Id);
   end;


   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (
      Value  : in     System.Address;
      Size   : in     Size_Type := Medium
     )
      return System.Address is
   begin
      return Get (Value => Value, Size => Size, Id => Processor_Id);
   end;



   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (
      Value : in     Integer_Address;
      Size  : in     Size_Type := Medium;
      Id    : in     Processor
     )
      return System.Address is

      Address : System.Address
        := MemMap (Address => Value, Size => Size, Id => Id);

   begin

      if Address = System.To_Address (Error) then

         raise Storage_Error;

      else

         return Address;

      end if;

   end;


   ----------------------------------------------------------------------------
   --
   -- Get
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Get
     (
      Value : in     System.Address;
      Size  : in     Size_Type := Medium;
      Id    : in     Processor)
      return System.Address is

   begin

      return Get (Value => To_Int_Addr ( Value ), Size => Size, Id => Id);

   end;


   ----------------------------------------------------------------------------
   --
   -- Rev
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Rev ( Address : in     System.Address ) return Integer_Address is

      Location : Integer_Address := MemRevMap ( Address => Address );

   begin

      if Location = 0 then

         raise Storage_Error;

      else

         return Location;

      end if;

   end;


   ----------------------------------------------------------------------------
   --
   -- MemRemove
   --
   -- Description:
   --    This function refers to a C function that removes all the shared
   --    memory segments.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MemRemove return Integer;
   pragma Import ( C, MemRemove, "memRemove" );


   ----------------------------------------------------------------------------
   --
   -- Free
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Free is

   begin

      if MemRemove = Error then

         raise Storage_Error;

      end if;

   end;


   ----------------------------------------------------------------------------
   --
   -- Initialize
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Initialize (Clear : in     Boolean) is
   begin

      if MemInit (Clear) = Error then

         raise Storage_Error;

      end if;

   end;


   ----------------------------------------------------------------------------
   --
   -- To_Access
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function To_Access (Source : System.Address) return Item_Type_Access is

      function To_Item_Type_Access is new Unchecked_Conversion
        (System.Address, Item_Type_Access );

   begin

      return To_Item_Type_Access
        (Get (Value => To_Int_Addr (Source), Size => Small));

   end;

   ----------------------------------------------------------------------------
   --
   -- MemSetPath
   --
   -- Description:
   --    This procedure refers to a C function that sets the path to the data
   --    location.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   procedure MemSetPath ( Path : in     Char_Array);
   pragma Import ( C, MemSetPath, "memSetPath" );


   ----------------------------------------------------------------------------
   --
   -- Set_Path
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   procedure Set_Path (Path : in     String) is

   begin
      MemSetPath (Path => To_C (Item =>Path));
   end;


   ----------------------------------------------------------------------------
   --
   -- MemSetPath
   --
   -- Description:
   --    This function refers to a C function that returns the path to the
   --    data location.
   --
   -- Exceptions Propagated:
   --    none
   --
   ----------------------------------------------------------------------------
   --
   function MemGetPath return Chars_Ptr;
   pragma Import ( C, MemGetPath, "memGetPath" );


   ----------------------------------------------------------------------------
   --
   -- Get_Path
   --
   -- See Package Spec for additional information
   --
   ----------------------------------------------------------------------------
   --
   function Get_Path return String is
   begin
      return To_Ada (Item => Value (Item => MemGetPath));
   end;

end Addresses;
