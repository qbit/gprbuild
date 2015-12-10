with Interfaces;

--------------------------------------------------------------------------------
--                          Common_Types Package Body                            --
--------------------------------------------------------------------------------

package body Common_Types is

  --++
  -- 
  --CHANGE LOG:
  --    Modifier      Date            Change Information
  --    --------      -------         ------------------
  --    she358        26-May-1998     Ported to VxWorks.
  ----

--------------------------------------------------------------------------------
--                              Definitions                                   --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--                     Unsigned Integer "and" Operation                       --
--------------------------------------------------------------------------------
 
  function "and" (P_Left, P_Right : in T_Unsigned_Integer) return T_Unsigned_Integer is
  begin
    return T_Unsigned_Integer(Interfaces."and"(Interfaces.Unsigned_32(P_Left), Interfaces.Unsigned_32(P_Right)));
  end "and";

--------------------------------------------------------------------------------
--                      Unsigned Integer "or" Operation                       --
--------------------------------------------------------------------------------
 
  function "or" (P_Left, P_Right : in T_Unsigned_Integer) return T_Unsigned_Integer is
  begin
    return T_Unsigned_Integer(Interfaces."or"(Interfaces.Unsigned_32(P_Left), Interfaces.Unsigned_32(P_Right)));
  end "or";

--------------------------------------------------------------------------------
--                     Unsigned Integer "not" Operation                       --
--------------------------------------------------------------------------------
 
  function "not" (P_Left : in T_Unsigned_Integer) return T_Unsigned_Integer is
  begin
    return T_Unsigned_Integer(Interfaces."not"(Interfaces.Unsigned_32(P_Left)));
  end "not";

--------------------------------------------------------------------------------
--                    Unsigned Short Integer "and" Operations                 --
--------------------------------------------------------------------------------
 
  function "and" (P_Left, P_Right : in T_Unsigned_Short_Integer) return T_Unsigned_Short_Integer is
  begin
    return T_Unsigned_Short_Integer(Interfaces."and"(Interfaces.Unsigned_16(P_Left), Interfaces.Unsigned_16(P_Right)));
  end "and";

--------------------------------------------------------------------------------
--                    Unsigned Short Integer "or" Operations                  --
--------------------------------------------------------------------------------
 
  function "or" (P_Left, P_Right : in T_Unsigned_Short_Integer) return T_Unsigned_Short_Integer is
  begin
    return T_Unsigned_Short_Integer(Interfaces."or"(Interfaces.Unsigned_16(P_Left), Interfaces.Unsigned_16(P_Right)));
  end "or";

--------------------------------------------------------------------------------
--                    Unsigned Short Integer "not" Operations                 --
--------------------------------------------------------------------------------
 
  function "not" (P_Left : in T_Unsigned_Short_Integer) return T_Unsigned_Short_Integer is
  begin
    return T_Unsigned_Short_Integer(Interfaces."not"(Interfaces.Unsigned_16(P_Left)));
  end "not";

--------------------------------------------------------------------------------
--                    Unsigned Tiny Integer "and" Operation                   --
--------------------------------------------------------------------------------
 
  function "and" (P_Left, P_Right : in T_Unsigned_Tiny_Integer)  return T_Unsigned_Tiny_Integer is
  begin
    return T_Unsigned_Tiny_Integer(Interfaces."and"(Interfaces.Unsigned_8(P_Left), Interfaces.Unsigned_8(P_Right)));
  end "and";

--------------------------------------------------------------------------------
--                    Unsigned Tiny Integer "or" Operation                    --
--------------------------------------------------------------------------------
 
  function "or" (P_Left, P_Right : in T_Unsigned_Tiny_Integer)  return T_Unsigned_Tiny_Integer is
  begin
    return T_Unsigned_Tiny_Integer(Interfaces."or"(Interfaces.Unsigned_8(P_Left), Interfaces.Unsigned_8(P_Right)));
  end "or";

--------------------------------------------------------------------------------
--                    Unsigned Tiny Integer "not" Operation                   --
--------------------------------------------------------------------------------
 
  function "not" (P_Left : in T_Unsigned_Tiny_Integer)  return T_Unsigned_Tiny_Integer is
  begin
    return T_Unsigned_Tiny_Integer(Interfaces."not"(Interfaces.Unsigned_8(P_Left)));
  end "not";

--------------------------------------------------------------------------------
--                          Integer "and" Operation                           --
--------------------------------------------------------------------------------

  function "and" (P_Left, P_Right : in T_Integer) return T_Integer is
  begin
    return T_Integer(Interfaces."and"(Interfaces.Unsigned_32(P_Left), Interfaces.Unsigned_32(P_Right)));
  end "and";

--------------------------------------------------------------------------------
--                          Integer "or" Operation                            --
--------------------------------------------------------------------------------

  function "or" (P_Left, P_Right : in T_Integer) return T_Integer is
  begin
    return T_Integer(Interfaces."or"(Interfaces.Unsigned_32(P_Left), Interfaces.Unsigned_32(P_Right)));
  end "or";

--------------------------------------------------------------------------------
--                          Integer "not" Operation                           --
--------------------------------------------------------------------------------

  function "not" (P_Left : in T_Integer) return T_Integer is
  begin
    return T_Integer(Interfaces."not"(Interfaces.Unsigned_32(P_Left)));
  end "not";

--------------------------------------------------------------------------------
--                       Short Integer "and" Operation                        --
--------------------------------------------------------------------------------
 
  function "and" (P_Left, P_Right : in T_Short_Integer) return T_Short_Integer is
  begin
    return T_Short_Integer(Interfaces."and"(Interfaces.Unsigned_16(P_Left), Interfaces.Unsigned_16(P_Right)));
  end "and";

--------------------------------------------------------------------------------
--                       Short Integer "or" Operation                         --
--------------------------------------------------------------------------------
 
  function "or" (P_Left, P_Right : in T_Short_Integer) return T_Short_Integer is
  begin
    return T_Short_Integer(Interfaces."or"(Interfaces.Unsigned_16(P_Left), Interfaces.Unsigned_16(P_Right)));
  end "or";

--------------------------------------------------------------------------------
--                       Short Integer "not" Operation                        --
--------------------------------------------------------------------------------
 
  function "not" (P_Left : in T_Short_Integer) return T_Short_Integer is
  begin
    return T_Short_Integer(Interfaces."not"(Interfaces.Unsigned_16(P_Left)));
  end "not";

--------------------------------------------------------------------------------
--                       Tiny Integer "and" Operation                         --
--------------------------------------------------------------------------------
 
  function "and" (P_Left, P_Right : in T_Tiny_Integer) return T_Tiny_Integer is
  begin
    return T_Tiny_Integer(Interfaces."and"(Interfaces.Unsigned_8(P_Left), Interfaces.Unsigned_8(P_Right)));
  end "and";

--------------------------------------------------------------------------------
--                       Tiny Integer "or" Operation                        --
--------------------------------------------------------------------------------
 
  function "or" (P_Left, P_Right : in T_Tiny_Integer) return T_Tiny_Integer is
  begin
    return T_Tiny_Integer(Interfaces."or"(Interfaces.Unsigned_8(P_Left), Interfaces.Unsigned_8(P_Right)));
  end "or";

--------------------------------------------------------------------------------
--                       Tiny Integer "not" Operation                         --
--------------------------------------------------------------------------------
 
  function "not" (P_Left : in T_Tiny_Integer) return T_Tiny_Integer is
  begin
    return T_Tiny_Integer(Interfaces."not"(Interfaces.Unsigned_8(P_Left)));
  end "not";

end Common_Types;
