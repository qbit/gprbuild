--                           Unclassified
----------------------------------------------------------------------------
--                       Common_Types Package Spec                        --
----------------------------------------------------------------------------
--**********************************************************************
--**                      Northrop Grumman                            **
--**********************************************************************
--
--                   AAA     SSSSS    CCCCC                      
--                  AA AA   SS   SS  CC   CC            
--                 AA   AA  SS       CC                   
--                 AAAAAAA   SSSSS   CC                  
--                 AA   AA       SS  CC                   
--                 AA   AA  SS   SS  CC   CC                 
--                 AA   AA   SSSSS    CCCCC                
--
--
-- Northrop Grumman Bethpage PRB Systems, Camarillo, CA
--
-- Program      : EA-18 
--
-- CSC          :  System common types
--
--++
-- 
--CHANGE LOG:
--    Modifier      Date            Change Information
--    --------      -------         ------------------
--    she358        26-May-1998     Ported to VxWorks.
--    sch467        12-Aug-2003     Merge Pt Mugu common changes
--    web698        01-Mar-2004     Changed name from PRB_Types to
--                                  Common_Types for EA-18G
--    web698        11-Mar-2005     Added some additional integer types
--                                  in this case are in use by UEU ICD
--    dgl696        29-Apr-2005     Added types to support angle in degrees
   --    D. Weblemoe   17-Aug-2005     Added additional unsigned 29 type
   --                                  in this case is in use by UEU ICD
-- 
--PURPOSE:
-- 
--    The purpose of this specification is to define common types that can be
--    used by all applications.  These types should isolate the applications 
--    from the compiler-defined types for each hardware platform.
--    
--IMPLICIT INPUTS/OUTPUTS:
-- 
--    None.
-- 
--SIDE EFFECTS:
-- 
--    None.
-- 
--USER INSTRUCTIONS:
-- 
--    None.
----
   
With Interfaces.C;    use type Interfaces.C.Unsigned;


Package Common_Types is
   
--------------------------------------------------------------------------------
--                              Definitions                                   --
--------------------------------------------------------------------------------

-- 1 Bit Unsigned Integer.
   type T_Unsigned_1 is new Integer range 0..2**1 - 1;
   for T_Unsigned_1'size use 1;

-- 2 Bit Unsigned Integer.
   type T_Unsigned_2 is new Integer range 0..2**2 - 1;
   for T_Unsigned_2'size use 2;

-- 3 Bit Unsigned Integer.
   type T_Unsigned_3 is new Integer range 0..2**3 - 1;
   for T_Unsigned_3'size use 3;

-- 4 Bit Unsigned Integer.
   type T_Unsigned_4 is new Integer range 0..2**4 - 1;
   for T_Unsigned_4'size use 4;

-- 5 Bit Unsigned Integer.
   type T_Unsigned_5 is new Integer range 0..2**5 - 1;
   for T_Unsigned_5'size use 5;

-- 6 Bit Unsigned Integer.
   type T_Unsigned_6 is new Integer range 0..2**6 - 1;
   for T_Unsigned_6'size use 6;

-- 7 Bit Unsigned Integer.
   type T_Unsigned_7 is new Integer range 0..2**7 - 1;
   for T_Unsigned_7'size use 7;

-- 8 Bit Unsigned Integer.
   type T_Unsigned_8 is new Integer range 0..2**8 - 1;
   for T_Unsigned_8'size use 8;

   subtype T_Unsigned_Byte is T_Unsigned_8;
   subtype T_Unsigned_Char is T_Unsigned_8;

-- 9 Bit Unsigned Integer.
   type T_Unsigned_9 is new Integer range 0..2**9 - 1;
   for T_Unsigned_9'size use 9;

-- 10 Bit Unsigned Integer.
   type T_Unsigned_10 is new Integer range 0..2**10 - 1;
   for T_Unsigned_10'size use 10;

-- 11 Bit Unsigned Integer.
   type T_Unsigned_11 is new Integer range 0..2**11 - 1;
   for T_Unsigned_11'size use 11;

-- 12 Bit Unsigned Integer.
   type T_Unsigned_12 is new Integer range 0..2**12 - 1;
   for T_Unsigned_12'size use 12;

-- 13 Bit Unsigned Integer.
   type T_Unsigned_13 is new Integer range 0..2**13 - 1;
   for T_Unsigned_13'size use 13;

-- 14 Bit Unsigned Integer.
   type T_Unsigned_14 is new Integer range 0..2**14 - 1;
   for T_Unsigned_14'size use 14;

-- 14 Bit Signed Integer.
   type T_Signed_14 is new Integer range -2**13..2**13 - 1;
   for T_Signed_14'size use 14;

-- 15 Bit Unsigned Integer.
   type T_Unsigned_15 is new Integer range 0..2**15 - 1;
   for T_Unsigned_15'size use 15;

-- 16 Bit Unsigned Integer.
   type T_Unsigned_16 is new Integer range 0..2**16 - 1;
   for T_Unsigned_16'size use 16;

-- 20 Bit Signed Integer.
   type T_Signed_20 is new Integer range -2**19..2**19 - 1;
   for T_Signed_20'size use 20;

-- 24 Bit Unsigned Integer.
   type T_Unsigned_24 is new Integer range 0..2**24 - 1;
   for T_Unsigned_24'size use 24;
   --
   -- 29 Bit Unsigned Integer.
      type T_Unsigned_29 is new Integer range 0..2**29 - 1;
      for T_Unsigned_29'size use 29;
   --
-- 30 Bit Signed Integer.
   type T_Signed_30 is new Integer range -2**29..2**29 - 1;
   for T_Signed_30'size use 30;

-- 32 Bit Unsigned Integer.
   -- type T_Unsigned_32 is mod 2**32;
   subtype T_Unsigned_32 is Interfaces.C.Unsigned;
   -- for T_Unsigned_32'size use 32;

-- 8 Bit Unsigned Integer.
   type T_Unsigned_Tiny_Integer is new Integer range 0..255;
   for T_Unsigned_Tiny_Integer'size use 8;

-- 16 Bit Unsigned Integer.
   type T_Unsigned_Short_Integer is new Integer range 0..65_535;
   for T_Unsigned_Short_Integer'size use 16;

-- 32 Bit Unsigned Integer.
-- This is not really a 32 bit unsigned integer. This is only a 31 Bit
-- unsigned integer.  A 32 bit unsigned integer would have a range of
-- 0..4_294_967_295.
--type T_Unsigned_Integer is new Integer range 0..4_294_967_295;
--       type T_Unsigned_Integer is mod 2**32;
   subtype T_Unsigned_Integer is Interfaces.C.Unsigned;
   -- for T_Unsigned_Integer'size use 32;


-- 8 Bit signed Integer.
   type T_Tiny_Integer is new Integer range -128..127;
   for T_Tiny_Integer'size use 8;

   subtype T_Tiny_Natural is T_Tiny_Integer range 0..T_Tiny_Integer'last;
   subtype T_Tiny_Positive is T_Tiny_Integer range 1..T_Tiny_Integer'last;

-- 16 Bit signed Integer.
   type T_Short_Integer is new short_Integer range -32_768..32_767;
   for T_Short_Integer'size use 16;

   subtype T_Short_Natural is T_Short_Integer range 0..T_Short_Integer'last;
   subtype T_Short_Positive is T_Short_Integer range 1..T_Short_Integer'last;

-- 32 Bit signed Integer.
   type T_Integer is new Integer range -2_147_483_648..2_147_483_647;
   for T_Integer'size use 32;

   subtype T_Natural is T_Integer range 0..T_Integer'last;
   subtype T_Positive is T_Integer range 1..T_Integer'last;
   
   -- positive Integer range 1..2**32-1
   subtype T_Positive_UINT is T_Unsigned_Integer range 
      1..T_Unsigned_Integer'Last;

-- 64 Bit Integers
   type T_Long_Long is range -(2**63)..(2**63-1);
   for T_Long_Long'size use 64;

   type T_ULong_Long is mod 2**64;
   for T_ULong_Long'size use 64;


-- Unsigned Integer arrays.
   type T_Unsigned_Tiny_Integer_Array is array (T_Integer range <>) of 
   T_Unsigned_Tiny_Integer;
   type T_Unsigned_Short_Integer_Array is array (T_Integer range <>) of 
   T_Unsigned_Short_Integer;
   type T_Unsigned_Integer_Array is array (T_Integer range <>) of 
   T_Unsigned_Integer;

   subtype T_Unsigned_Byte_Array is T_Unsigned_Tiny_Integer_Array;
   subtype T_Unsigned_Char_Array is T_Unsigned_Tiny_Integer_Array;

-- 32 Bit floating point real, at least 6 digits of precision.
   subtype T_Short_Float is Float;

-- 64 Bit floating point real, at least 15 digitsof precision.
   subtype T_Float is Long_Float;

-- reserve T_Long_Float for a 128 Bit real - when needed

-- Access to a boolean (for commonality)
   type T_A_Boolean is access Boolean;

-- String of length 8
   subtype T_String_8 is String(1..8);
   
-- String of length 128
   subtype T_String_128 is String(1..128);
   
   --8 bit array
   Type T_8_BIT_U1_Array       is array (0.. 7)  of Common_Types.T_Unsigned_1;
   for T_8_BIT_U1_Array'size use 8;

--------------------------------------------------------------------------------
--                     Binary angle (BAMS) support                            --
--------------------------------------------------------------------------------
-- 8  Bit mod type for 8 bit bams
   type T_Bams_8 is mod 2**8;
   for T_Bams_8'size use 8;

-- 16 Bit mod type for 16 bit bams
   type T_Bams_16 is mod 2**16;
   for T_Bams_16'size use 16;

-- 32 Bit mod type for 32 bit bams
   type T_Bams_32 is mod 2**32;
   for T_Bams_32'size use 32;
   
--------------------------------------------------------------------------------
--                     Angle (Degrees) support                            --
--------------------------------------------------------------------------------
-- Angle types
   subtype T_Angle      is integer range      0 .. 359;   -- degrees
   
 -- Angle type as a mod of 360
   type T_Angle_Mod     is mod 360;   -- degrees
   subtype T_Bearing    is T_Angle_Mod;

   
--------------------------------------------------------------------------------
--                            Bitwise Operations                              --
--------------------------------------------------------------------------------

-- T_Unsigned_Tiny_Integer Bitwise Operations.
    function "and" (P_Left, P_Right : in T_Unsigned_Tiny_Integer) 
               return T_Unsigned_Tiny_Integer;
    function "or"  (P_Left, P_Right : in T_Unsigned_Tiny_Integer) 
               return T_Unsigned_Tiny_Integer;
    function "not" (P_Left : in T_Unsigned_Tiny_Integer) 
               return T_Unsigned_Tiny_Integer;

-- T_Unsigned_Short_Integer Bitwise Operations.
    function "and" (P_Left, P_Right : in T_Unsigned_Short_Integer) 
               return T_Unsigned_Short_Integer;
    function "or"  (P_Left, P_Right : in T_Unsigned_Short_Integer) 
               return T_Unsigned_Short_Integer;
    function "not" (P_Left : in T_Unsigned_Short_Integer) 
               return T_Unsigned_Short_Integer;

-- T_Unsigned_Integer Bitwise Operations.
    function "and" (P_Left, P_Right : in T_Unsigned_Integer) 
               return T_Unsigned_Integer;
    function "or"  (P_Left, P_Right : in T_Unsigned_Integer) 
               return T_Unsigned_Integer;
    function "not" (P_Left : in T_Unsigned_Integer) 
               return T_Unsigned_Integer;

-- T_Integer Bitwise Operations.
    function "and" (P_Left, P_Right : in T_Integer) 
               return T_Integer;
    function "or"  (P_Left, P_Right : in T_Integer) 
               return T_Integer;
    function "not" (P_Left : in T_Integer) 
               return T_Integer;

-- T_Short_Integer Bitwise Operations.
    function "and" (P_Left, P_Right : in T_Short_Integer) 
               return T_Short_Integer;
    function "or"  (P_Left, P_Right : in T_Short_Integer) 
               return T_Short_Integer;
    function "not" (P_Left : in T_Short_Integer) 
               return T_Short_Integer;

-- T_Tiny_Integer Bitwise Operations.
    function "and" (P_Left, P_Right : in T_Tiny_Integer) 
               return T_Tiny_Integer;
    function "or"  (P_Left, P_Right : in T_Tiny_Integer) 
               return T_Tiny_Integer;
    function "not" (P_Left : in T_Tiny_Integer) 
               return T_Tiny_Integer;

   Type T_20_unsigned_32 is array (0..19) of T_Unsigned_32;


end Common_Types;
