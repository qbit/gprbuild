-------------------------------------------------------------------------------
-- UNIT NAME       : tiu_coding
-- UNIT KIND       : Body package
-- FILENAME        : tiu_coding.adb
-- HIERARCHY       : TIU_APPLICATION/tiu_interface/tiu_coding
-- SYSTEM          : TIU Application
-- CLASSIFICATION  : SIL 4
-- COPYRIGHT       : ALSTOM
--
-- IMPLEMENTATION DOCUMENTATION
--
-- DEROGATION:
--
-- HISTORY:
--#ACD# M(HIST) Only the high level modifications are handled here
--  VERSION     DATE        NAME        DETAILS
--  2.0.02      01/10/03    MDE         ERTMS00000827 : Added canape functions to ASW
--  3.1         15/12/04    MDE         ERTMS00005352 : Added numbering of messages.
--  5.1         22/09/06    ChD         ERTMS00014466 : Some conversion problems during conversions from variant records into Bit array
--  6.0         20/06/07    ChD         ERTMS00015728 : canape correction (no more error when splitted plug)
--#end ACD#
-------------------------------------------------------------------------------
with Text_Io;

package body Unit1.Child is

  procedure Display_Auxiliary_Text is
  begin
    Text_Io.Put_Line (Auxiliary_Text_To_Display_C);
  end Display_Auxiliary_Text;
end Unit1.Child;

with Text_Io;
package body Unit1 is
  procedure Display_Title is
  begin
    Text_Io.Put_Line ("Test Multiple-Units with Multi-Libraries");
    Text_Io.Put_Line ("----------------------------------------");
  end Display_Title;
end Unit1;
