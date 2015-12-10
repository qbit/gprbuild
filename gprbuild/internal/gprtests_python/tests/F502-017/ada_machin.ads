-- pragma C_Pass_By_Copy(128);

with Interfaces.C;

package Ada_Machin is

   --  This type maps to C "unsigned short".
   subtype Unsigned_Short_Type is Interfaces.C.unsigned_short;
   type Unsigned_Short_Type_Ref is access all Unsigned_Short_Type;
   for Unsigned_Short_Type_Ref'Storage_Size use 0;

   --  This type maps to C "unsigned long".
   subtype Unsigned_Long_Type is Interfaces.C.unsigned_long;
   type Unsigned_Long_Type_Ref is access all Unsigned_Long_Type;
   for Unsigned_Long_Type_Ref'Storage_Size use 0;

   --  This type maps to C "float".
   subtype C_Float_Type is Interfaces.C.C_float;
   type C_Float_Type_Ref is access all C_Float_Type;
   for C_Float_Type_Ref'Storage_Size use 0;


   type C_struct is
      record
         a1 : Unsigned_Short_Type;
         a2 : Unsigned_Long_Type;
      end record;
   pragma Convention (C_Pass_By_Copy, C_struct);

   procedure Put_System_Data (System_Data : in C_struct);
   pragma export (C, Put_System_Data, "fonction_ada");

   procedure Put_System_Data_Float(System_Data : in C_Float_Type);
   pragma export (C, Put_System_Data_Float, "fonction_ada_float");


end Ada_Machin;

