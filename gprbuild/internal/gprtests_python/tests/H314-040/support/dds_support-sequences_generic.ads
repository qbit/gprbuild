pragma Ada_05;
private with System;
private with GNAT.Source_Info;

generic
   type Element is private;
   type Element_Access is access all Element;
   type Index_Type is range <>;
   First_Element : Index_Type;
   type Element_Array is array (Index_Type range <>) of aliased Element;
   with procedure Initialize (Self  : in out Element) is <>;
   with procedure Finalize (Self  : in out Element) is <>;
   with procedure Copy (Dst : in out Element; Src : in Element) is <>;
package DDS_Support.Sequences_Generic is
--  This package implemnts Sequences as defined by
--  the RTI C Implementation of Sequences.
--  The implementation maps 1 to 1 to the C implementation in order to
--  make data intercaangeble between tha languages.
--  This package may later be replaces by Ada.Containers.Vectors but that would
--  require rewriting of the serialization and deserialisation code.

   type Sequence is private;
   --  <TBD>

   DEFAULT_SEQUENCE : constant Sequence;
   --  Used for initialization w/aggregate assignment


   procedure Initialize
     (Self : not null access Sequence);
   --  Initialize the structure for future use

   function Get_Reference (Self  : not null access constant Sequence;
                           Index : Index_Type) return Element_Access;
   --  Get a reference to the Index-th element of the sequence.
   --  Raises Constraint_Error if index is out of bounds.

   procedure Set_Element (Self  : not null access Sequence;
                          Index : Index_Type;
                          Elt   : Element);
   --  Sets the Index-th element of the sequence to given Element
   --  Raises Constraint_Error if index is out of bounds.

   function Get (Self  : not null access constant Sequence;
                 Index : Index_Type) return Element;
   --  Get the Index-th element of the sequence.
   --  Raises Constraint_Error if index is out of bounds.

   function Get_Element_Pointers_Allocation (Self : not null access constant Sequence)
                                             return Boolean;
   --  Returns the Element_Pointers_Allocation

   procedure Set_Element_Pointers_Allocation  (Self              : not null access Sequence;
                                               Allocate_Pointers : in Boolean);
   --  Sets the Element_Pointers_Allocation


   function Get_Maximum (Self : not null access constant Sequence) return Index_Type;
   --  Get the current maximum of the sequence.

   procedure Set_Maximum (Self    : not null access Sequence;
                          New_Max : in Index_Type);
   --  Resize this sequence to a new desired maximum.

   function Get_Length (Self : not null access constant Sequence) return Index_Type;
   --  Get the sequence length.

   procedure Set_Length (Self       : not null access Sequence;
                         New_Length : in Index_Type);
   --  Change the length of this sequence.
   --  Will raise Constraint_Error if new length exceeds Max_Length.

   procedure Ensure_Length (Self   : not null access Sequence;
                            Length : in Index_Type;
                            Max    : in Index_Type);
   --  Set the sequence to the desired length, and resize the sequence if necessary.
   --  Raises:
   --    Program_Error On Illegal use
   --    Storage_Errro if no memory is avalible.



   procedure Copy_No_Alloc (Self : not null access Sequence;
                            Src  : not null access constant Sequence);
   --  Copy elements from another sequence, only if the destination sequence has enough capacity.

   procedure Copy (Self : not null access Sequence;
                   Src  : not null access constant Sequence);
   --  Copy elements from another sequence, resizing the sequence if necessary.

   procedure From_Array (Self : not null access Sequence;
                         Src  : in Element_Array);
   --  Copy elements from an array of elements, resizing the sequence if necessary.
   --  The original contents of the sequence (if any) are replaced.

   procedure To_Array (Self   : not null access constant Sequence;
                       Target : out Element_Array);
   --  Copy elements to an array of elements. The original contents of the array (if any) are replaced.
   --  Raises constraint error if the target array is to small.

   function To_Array (Self   : not null access constant Sequence) return Element_Array;
   --  Convert elements to an array of elements.

   procedure Loan_Contiguous (Self       : not null access Sequence;
                              Buffer     : not null access Element_Array;
                              New_Length : in Index_Type;
                              New_Max    : in Index_Type);
   --  Loan a contiguous buffer to this sequence.

   procedure Loan_Discontiguous (Self       : access Sequence;
                                 Buffer     : not null access Element_Access;
                                 New_Length : in Index_Type;
                                 New_Max    : in Index_Type);
   --  Loan a discontiguous buffer to this sequence.

   procedure Unloan  (Self : not null access Sequence);
   --  Return the loaned buffer in the sequence.

   function Has_Ownership (Self : not null access constant Sequence) return Boolean;
   --  The value of the owned flag.

   procedure Finalize (Self : not null access Sequence);
   --  Deallocate this sequence's buffer.

   SEQUENCE_UNINITIALIZED : exception;
   SEQUENCE_ERROR : exception;
private
   SEQUENCE_MAGIC_NUMBER : constant := 16#7344#;

   type Memory_Element_Pointer  is array (First_Element .. Index_Type'Last) of aliased Element_Access;
   subtype Memory_Element_Array is Element_Array (First_Element .. Index_Type'Last);

   type Memory_Element_Array_Pointer is access all Memory_Element_Array;
   pragma No_Strict_Aliasing (Memory_Element_Array_Pointer);

   type Element_Array_Access is access all Element_Array;
   type NElement_Access is new Element_Access;
   type Memory_Element_Pointer_Access is access all Memory_Element_Pointer;
   type Sequence is record
      Owned                     : Boolean := True;
      Contiguous_Buffer         : Memory_Element_Array_Pointer := null;
      Discontiguous_Buffer      : Memory_Element_Pointer_Access := null;
      Maximum                   : Index_Type := 0;
      Length                    : Index_Type := 0;
      Sequence_Init             : Integer := SEQUENCE_MAGIC_NUMBER;
      Read_Token1               : System.Address := System.Null_Address;
      Read_Token2               : System.Address := System.Null_Address;
      ElementPointersAllocation : Boolean := True;
   end record;
   pragma Convention (C, Sequence);

   DEFAULT_SEQUENCE : constant Sequence :=
                       (Owned => True,
                         Contiguous_Buffer => null,
                         Discontiguous_Buffer => null,
                         Maximum              => 0,
                         Length               => 0,
                         Sequence_Init        => SEQUENCE_MAGIC_NUMBER,
                         Read_Token1          => System.Null_Address,
                         Read_Token2          => System.Null_Address,
                         ElementPointersAllocation => True);

   procedure Check_InvariantsI (Self             : not null access constant Sequence;
                                Calling_Function : Standard.String := GNAT.Source_Info.Enclosing_Entity);
   procedure Check_InitI (Self : not null access constant Sequence);

   procedure Copy_No_AllocI (Self : not null access Sequence;
                             Src  : not null access constant Sequence);

   procedure Get_Read_TokenI (Self   : not null access constant Sequence;
                              Token1 : not null access System.Address;
                              Token2 : not null access System.Address);

   procedure Set_Read_TokenI (Self   : not null access Sequence;
                              Token1 : in System.Address;
                              Token2 : in System.Address);

   function Get_Contiguous_BufferI  (Self : not null access constant Sequence)
                                     return access Memory_Element_Array;

   function Get_DisContiguous_BufferI (Self : not null access constant Sequence)
                                       return access Memory_Element_Pointer;
end DDS_Support.Sequences_Generic;
