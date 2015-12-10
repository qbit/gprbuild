pragma Ada_05;
pragma Assertion_Policy (Check);
with System.Memory; use System.Memory;
with Ada.Unchecked_Conversion;
package body DDS_Support.Sequences_Generic is

   -----------------
   -- Check_InitI --
   -----------------

   procedure Check_InitI (Self : not null access constant Sequence) is
   begin
      if Self.Sequence_Init /= SEQUENCE_MAGIC_NUMBER then
         raise SEQUENCE_UNINITIALIZED;
      end if;
   end Check_InitI;

   ----------------------------
   -- Get_Contiguous_BufferI --
   ----------------------------

   function Get_Contiguous_BufferI
     (Self : not null access constant Sequence)
      return access Memory_Element_Array is
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);
      return Self.Contiguous_Buffer;
   end Get_Contiguous_BufferI;


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Sequence)
   is
   begin
      Self.Owned := True;
      Self.Contiguous_Buffer := null;
      Self.Discontiguous_Buffer := null;
      Self.Maximum := 0;
      Self.Length := 0;
      Self.Sequence_Init := SEQUENCE_MAGIC_NUMBER;
      Self.Read_Token1 := System.Null_Address;
      Self.Read_Token2 := System.Null_Address;
      Self.ElementPointersAllocation := True;
   end Initialize;

   -------------------------------------
   -- Get_Element_Pointers_Allocation --
   -------------------------------------

   function Get_Element_Pointers_Allocation
     (Self : not null access constant Sequence)
      return Boolean
   is
   begin
      return Self.ElementPointersAllocation;
   end Get_Element_Pointers_Allocation;

   -------------------------------------
   -- Set_Element_Pointers_Allocation --
   -------------------------------------

   procedure Set_Element_Pointers_Allocation
     (Self              : not null access Sequence;
      Allocate_Pointers : in Boolean)
   is
   begin
      Self.ElementPointersAllocation := Allocate_Pointers;
   end Set_Element_Pointers_Allocation;

   -----------------
   -- Get_Maximum --
   -----------------

   function Get_Maximum
     (Self : not null access constant Sequence)
      return Index_Type
   is
   begin
      Check_InvariantsI (Self);
      return Index_Type (Self.Maximum);
   end Get_Maximum;

   -----------------
   -- Set_Maximum --
   -----------------

   procedure Set_Maximum
     (Self : not null access Sequence; New_Max : Index_Type)
   is
      NewBuffer            : Memory_Element_Array_Pointer;
      NewBuffer_As_Address : System.Address;
      for NewBuffer_As_Address'Address use NewBuffer'Address;

      OldBuffer            : Memory_Element_Array_Pointer;
      OldBuffer_As_Address : System.Address;
      for OldBuffer_As_Address'Address use OldBuffer'Address;

      NewLength            : Index_Type := 0;
      OldMaxLength         : Index_Type := 0;

   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);

      if not Self.Owned then
         raise SEQUENCE_ERROR with "buffer must not be loaned";
      end if;
      if Self.Discontiguous_Buffer /= null then
         raise SEQUENCE_ERROR with "discontiguous buffer is not NULL!";
      end if;

      --  Bail out now if no change
      if New_Max = Index_Type (Self.Maximum) then
         return;
      end if;

      --  Allocate new contiguous buffer
      if New_Max > 0 then
         NewBuffer_As_Address := System.Memory.Alloc (size_t (New_Max) * Element'Object_Size / 8);
         for I in First_Element .. New_Max loop
            Initialize (NewBuffer (I));
         end loop;

      end if;

      --  Copy elements to new buffer
      if Self.Length < New_Max then
         NewLength := Self.Length;
      else
         NewLength := New_Max;
      end if;

      pragma Assert (not (NewLength > 0 and Self.Contiguous_Buffer = null), "inconsistent buffer state");

      for I in 1 .. NewLength loop
         Copy (NewBuffer (I), Self.Contiguous_Buffer (I));
      end loop;

      OldBuffer := Self.Contiguous_Buffer;
      Self.Contiguous_Buffer := NewBuffer.all'Unrestricted_Access;
      OldMaxLength := Self.Maximum;
      Self.Maximum := New_Max;
      Self.Length := NewLength;

      if OldBuffer /= null then
         for I in 0 .. OldMaxLength loop
            Finalize (OldBuffer (I));
         end loop;
         Free (OldBuffer_As_Address);
      end if;
   end Set_Maximum;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Self : not null access constant Sequence) return Index_Type is
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);
      return Self.Length;
   end Get_Length;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Self : not null access Sequence; New_Length : Index_Type)
   is
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);
      if New_Length > Self.Maximum then
         raise Constraint_Error with "Maximum length exeded.";
      end if;
      Self.Length := New_Length;
      Check_InvariantsI (Self);
   end Set_Length;

   -------------------
   -- Ensure_Length --
   -------------------

   procedure Ensure_Length
     (Self   : not null access  Sequence;
      Length : Index_Type;
      Max    : Index_Type)
   is
      CurrMax         : Index_Type;
   begin
      if Length > Max then
         raise Program_Error with  "Length > Max.";
      end if;
      --  Check if we need to resize ---
      CurrMax := Get_Maximum (Self);
      if CurrMax >= Length then
         --  Sequence has enough room: just set the length
         Set_Length (Self, Length);
      elsif Has_Ownership (Self) then
         --  Sequence needs more memory. We own our buffer, so we can
         --  go ahead and allocate more and then set the length.
         Set_Maximum (Self, Max);
         Set_Length (Self, Length);
      else
         raise Program_Error with "Sequence must be owned";
      end if;
   end Ensure_Length;

   ---------
   -- Get --
   ---------

   function Get
     (Self  : not null access constant Sequence;
      Index : Index_Type)
      return Element
   is
   begin
      return Get_Reference (Self, Index).all;
   end Get;

   -------------------
   -- Get_Reference --
   -------------------
   function Get_Reference
     (Self  : not null access constant Sequence;
      Index : Index_Type)
      return Element_Access
   is
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);
      if Index > Self.Length then
         raise Constraint_Error with "index out of bounds";
      end if;

      if  (Index) > Self.Length then
         return null;
      end if;
      if Self.Discontiguous_Buffer /= null then
         return Self.Discontiguous_Buffer.all (Index);
      else
         return Self.Contiguous_Buffer.all (Index)'Access;
      end if;
   end Get_Reference;

   -----------------
   -- Set_Element --
   -----------------

   procedure Set_Element (Self  : not null access Sequence;
                          Index : Index_Type;
                          Elt   : Element)
   is
      Elt_Ref : Element_Access;
   begin
      Elt_Ref := Get_Reference (Self, Index);
      Elt_Ref.all := Elt;
   end Set_Element;

   -------------------
   -- Copy_No_Alloc --
   -------------------

   procedure Copy_No_Alloc
     (Self : not null access Sequence;
      Src  : not null access constant Sequence)
   is
   begin
      Check_InitI (Self);
      pragma Assert (Has_Ownership (Self), "Sequence not owner.");
      Check_InvariantsI (Self);
      Copy_No_AllocI (Self, Src);
   end Copy_No_Alloc;

   procedure Copy_No_AllocI (Self : not null access Sequence;
                             Src  : not null access constant Sequence) is

      Length : Index_Type;
   begin
      --      /* memory can either be owned not not owned, as long as the maximum
      --         length is large enough */
      --
      --      /* --- Check source initialization --- */
      --      /* Make sure that the sequence from which we're copying doesn't contain
      --       * garbage values. If it does, consider it empty
      --       * as in TSeq_check_initI(). (We can't actually initialize it here
      --       * because it's const.)
      --       */
      if  Src.Sequence_Init = SEQUENCE_MAGIC_NUMBER then
         Length := Src.Length;
      end if;

      --
      --      /* --- Copy length --- */
      --      /* set_length() will check the new length against our max; it will
      --       * also make sure the length is positive (i.e. that src is properly
      --       * initialized).
      --       */
      Set_Length (Self, Length);
      --
      --      /* --- Copy elements --- */
      --      /* This sequence may be using its contiguous buffer or its discontiguous
      --       * buffer; the same is true of the other sequence.
      --       */
      if Self.Contiguous_Buffer /= null then
         if Src.Contiguous_Buffer /= null then
            for I in 1 .. Length loop
               Self.Contiguous_Buffer (I) := Src.Contiguous_Buffer (I);
            end loop;
         else
            for I in 1 .. Length loop
               Self.Contiguous_Buffer (I) := Src.Discontiguous_Buffer (I).all;
            end loop;
         end if;
      else
         if (Src.Contiguous_Buffer /= null) then
            for I in 1 .. Length loop
               Self.Discontiguous_Buffer (I).all := Src.Contiguous_Buffer (I);
            end loop;
         else
            for I in 1 .. Length loop
               Self.Discontiguous_Buffer (I).all := Src.Discontiguous_Buffer (I).all;
            end loop;
         end if;
      end if;
      Check_InvariantsI (Self);
   end Copy_No_AllocI;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Self : not null access Sequence;
      Src  : not null access constant Sequence)
   is
      MyMax  : Index_Type := 0;
      SrcMax : Index_Type  := 0;
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);

      --  Copy_from doesn't allocate memory. We need to make sure
      --  the sequence is large enough
      MyMax := Get_Maximum (Self);
      if MyMax < Get_Length (Src) then
         --  Use get_maximum() instead of get_length() to align with the
         --  copy constructor and avoid memory fragmentation
         --   * when several assignments are being made to the same variable.
         SrcMax := Get_Maximum (Src);
         Set_Maximum (Self, SrcMax);
      end if;

      Copy_No_Alloc (Self, Src);
      Check_InvariantsI (Self);
   end Copy;

   ----------------
   -- From_Array --
   ----------------

   procedure From_Array
     (Self : not null access Sequence;
      Src  : in Element_Array)
   is
      ArraySeq : aliased Sequence;
   begin
      --  Loan the array to a local sequence on the stack
      Loan_Contiguous (ArraySeq'Access,
                       Src'Unrestricted_Access,
                       Src'Length, Src'Length);
      --  Copy into this sequence, resizing if necessary
      Copy (Self, ArraySeq'Access);
      --  Unloan the array from the local sequence on the stack
      Unloan (ArraySeq'Access);
   end From_Array;

   --------------
   -- To_Array --
   --------------
   procedure To_Array
     (Self   : not null access constant Sequence;
      Target : out Element_Array)
   is
   begin
      pragma Assert (Target'Length = Self.Length, "Length mismatch");
      Target := Self.Contiguous_Buffer.all (1 .. Self.Length);
   end To_Array;

   function To_Array
     (Self   : not null access constant Sequence) return Element_Array is
      Empty : Element_Array (1 .. 0);
   begin
      if Self.Contiguous_Buffer = null or else Self.Length = 0 then
         return Empty;
      end if;


      return Self.Contiguous_Buffer.all (1 .. Self.Length);
   end To_Array;

   ---------------------
   -- Loan_Contiguous --
   ---------------------

   procedure Loan_Contiguous
     (Self       : not null access Sequence;
      Buffer     : not null access Element_Array;
      New_Length : in Index_Type;
      New_Max    : in Index_Type)
   is
      function Convert is new Ada.Unchecked_Conversion (System.Address, Memory_Element_Array_Pointer);

      LBuffer : constant Memory_Element_Array_Pointer := Convert (Buffer.all'Address);
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);

      pragma Assert (Self.Maximum = 0, "max size must be 0");
      pragma Assert (New_Length < New_Max, "Insufficent space");
      pragma Assert (New_Max = 0 and Buffer = null, "NULL buffer can't have non-zero maximum");

      --      /* --- Load buffer --- */
      Self.Contiguous_Buffer := LBuffer;
      Self.Length := New_Length;
      Self.Maximum := New_Max;
      Self.Owned := False;

      Check_InvariantsI (Self);
   end Loan_Contiguous;

   ------------------------
   -- Loan_Discontiguous --
   ------------------------

   procedure Loan_Discontiguous
     (Self       : access Sequence;
      Buffer     : not null access Element_Access;
      New_Length : in Index_Type;
      New_Max    : in Index_Type)
   is
      type Element_Access_Access is access all Element_Access;
      type Memory_Element_Pointer_Access is access all Memory_Element_Pointer;
      function Convert is new Ada.Unchecked_Conversion (Element_Access_Access, Memory_Element_Pointer_Access);
      LBuffer : constant access Memory_Element_Pointer := Convert (Buffer.all'Unrestricted_Access);

   begin

      Check_InitI (Self);
      Check_InvariantsI (Self);

      pragma Assert (Self.Maximum = 0, "max size must be 0");
      pragma Assert (New_Length < New_Max, "Insufficent space");
      pragma Assert (New_Max = 0 and Buffer = null, "NULL buffer can't have non-zero maximum");

      Self.Discontiguous_Buffer := LBuffer.all'Unrestricted_Access;
      Self.Length := New_Length;
      Self.Maximum := New_Max;
      Self.Owned := False;

      Check_InvariantsI (Self);
   end Loan_Discontiguous;

   ------------
   -- Unloan --
   ------------

   procedure Unloan
     (Self : not null access Sequence)
   is
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);

      pragma Assert (not Self.Owned, "buffer must be loaned");

      Self.Contiguous_Buffer := null;
      Self.Discontiguous_Buffer := null;
      Self.Maximum := 0;
      Self.Length := 0;
      Self.Owned := True;

      Check_InvariantsI (Self);
   end Unloan;

   ---------------------------------
   --  Get_DisContiguous_BufferI  --
   ---------------------------------

   function Get_DisContiguous_BufferI
     (Self : not null access constant Sequence)
      return access Memory_Element_Pointer is
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);
      return Self.Discontiguous_Buffer;
   end Get_DisContiguous_BufferI;

   -------------------
   -- Has_Ownership --
   -------------------

   function Has_Ownership
     (Self : not null access constant Sequence)
      return Boolean
   is
   begin
      Check_InitI (Self);
      Check_InvariantsI (Self);
      return Self.Owned;
   end Has_Ownership;

   ---------------------
   -- Get_Read_TokenI --
   ---------------------

   procedure Get_Read_TokenI (Self   : not null access constant Sequence;
                              Token1 : not null access System.Address;
                              Token2 : not null access System.Address) is
   begin
      --  Check preconditions
      Check_InitI (Self);
      Check_InvariantsI (Self);
      --  Get token
      Token1.all := Self.Read_Token1;
      Token2.all := Self.Read_Token2;
   end Get_Read_TokenI;

   ---------------------
   -- Set_Read_TokenI --
   ---------------------

   procedure Set_Read_TokenI (Self   : not null access Sequence;
                              Token1 : in System.Address;
                              Token2 : in System.Address) is
   begin
      --  Check preconditions
      Check_InitI (Self);
      Check_InvariantsI (Self);
      --  Get token
      Self.Read_Token1 := Token1;
      Self.Read_Token2 := Token2;
   end Set_Read_TokenI;

   -----------------------
   -- Check_InvariantsI --
   -----------------------

   procedure Check_InvariantsI
     (Self             : not null access constant Sequence;
      Calling_Function : Standard.String := GNAT.Source_Info.Enclosing_Entity) is
      pragma Unreferenced (Calling_Function);
   begin
      pragma Assert (not (Self.Contiguous_Buffer /= null and Self.Discontiguous_Buffer /= null),
                     "invariant: both buffers are non-NULL");

      pragma Assert (Self.Length <= Self.Maximum, "invariant: length > maximum");

      pragma Assert (not (Self.Owned and (Self.Discontiguous_Buffer /= null)),
              "invariant: sequence owns memory but has discontiguous buffer");

      pragma Assert (not (Self.Maximum > 0 and then
          (Self.Contiguous_Buffer = null and Self.Discontiguous_Buffer = null)),
        "invariant: maximum > 0 but buffer is NULL");
      null;
   end Check_InvariantsI;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Self : not null access Sequence)
   is
   begin
      --      return TSeq_set_maximum(self, 0);
      Set_Maximum (Self, 0);
   end Finalize;

end DDS_Support.Sequences_Generic;
