pragma Ada_05;
with Interfaces.C;
with Interfaces.C.Strings;

package RTI.Cdr is
   pragma Preelaborate;

   type Stream is private;

   type TypeCode is private;

private

   use Interfaces;

   type ValueModifier is new Interfaces.Integer_16;

   type Visibility is new Interfaces.Integer_16;

   type Integer_32_Ptr is access Interfaces.Integer_32;

   type TypeCode_Access is access TypeCode;

   type TypeCodeMember is record
      T_Name : Interfaces.C.Strings.chars_ptr;
      T_IsPointer : Interfaces.Unsigned_8;
      T_Bits : Interfaces.Integer_16;
      T_Type_Code : TypeCode_Access;
      T_Ordinal : Interfaces.Integer_32;
      T_LabelsCount : Interfaces.Unsigned_32;
      T_Label : Interfaces.Integer_32;
      T_Labels : Integer_32_Ptr;
      T_IsKey : Interfaces.Unsigned_8;
      T_Visibility : Visibility;
   end record;
   pragma Convention (C, TypeCodeMember);

   type TypeCodeMember_Access is access TypeCodeMember;

   type TypeCode is record
      T_Kind : Interfaces.Integer_32;
      T_IsPointer : Interfaces.Unsigned_8;
      T_Default_Index : Interfaces.Integer_32;
      T_Name : Interfaces.C.Strings.chars_ptr;
      T_TypeCode : TypeCode_Access;
      T_MaximumLength  : Interfaces.Integer_32;
      T_DimensionsCount : Interfaces.Integer_32;
      T_Dimensions : Integer_32_Ptr;
      T_MemberCount : Interfaces.Integer_32;
      T_Members : TypeCodeMember_Access;
      T_TypeModifier : ValueModifier;
   end record;
   pragma Convention (C, TypeCode);


   type Stream  is record
      Buffer          : access C.char;
      BufferLength    : C.int;
      CurrentPosition : access C.char;
      NeedByteSwap    : C.unsigned_char;
      Endian          : C.unsigned_char;
      BitBuffer       : C.unsigned_long;
      BitBufferLength : C.int;
   end record;
   pragma Convention (C, Stream);

end RTI.Cdr;
