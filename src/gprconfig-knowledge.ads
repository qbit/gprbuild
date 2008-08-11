------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R C O N F I G                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2006-2007, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This unit is responsible for parsing the gprconfig knowledge base.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Regpat;
with Namet;

package GprConfig.Knowledge is

   Verbose_Level : Natural := 0;
   --  What level of debug traces to display. The higher, the more messages
   --  we show

   Quiet_Output : Boolean := False;
   --  Whether or not to display any message other than error messages

   Generate_Error : exception;
   --  To be raised when an error occurs during generation of config files.

   Invalid_Knowledge_Base : exception;
   --  To be raised when an error occurred while parsing the knowledge base

   type Knowledge_Base is private;

   procedure Parse_Knowledge_Base
     (Base : out Knowledge_Base; Directory : String);
   --  Parse info from the knowledge base, and store it in memory.
   --  Only information relevant to the current host is parsed.

   type Targets_Set_Id is range -1 .. Natural'Last;
   --  Identify a target aliases set.

   All_Target_Sets     : constant Targets_Set_Id := -1;
   --  Matches all target sets

   Unknown_Targets_Set : constant Targets_Set_Id := 0;
   --  Special target set when a target is not known.

   subtype Known_Targets_Set_Id is Targets_Set_Id
     range 1 .. Targets_Set_Id'Last;
   --  Known targets set.  They are in the base.

   function Hash_Case_Insensitive
     (Name : Namet.Name_Id) return Ada.Containers.Hash_Type;
   package Variables_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Namet.Name_Id,
      Element_Type    => Namet.Name_Id,
      Hash            => Hash_Case_Insensitive,
      Equivalent_Keys => Namet."=",
      "="             => Namet."=");

   type Compiler is record
      Name        : Namet.Name_Id := Namet.No_Name;
      Executable  : Namet.Name_Id := Namet.No_Name;
      Target      : Namet.Name_Id := Namet.No_Name;
      Targets_Set : Targets_Set_Id;
      Path        : Namet.Name_Id := Namet.No_Name;
      Base_Name   : Namet.Name_Id := Namet.No_Name;
      Version     : Namet.Name_Id := Namet.No_Name;
      Variables   : Variables_Maps.Map;
      Prefix      : Namet.Name_Id := Namet.No_Name;
      Runtime     : Namet.Name_Id := Namet.No_Name;
      Runtime_Dir : Namet.Name_Id := Namet.No_Name;
      Path_Order  : Integer;

      Language_Case : Namet.Name_Id := Namet.No_Name;
      --  The supported language, with the casing read from the compiler. This
      --  is for display purposes only

      Language_LC : Namet.Name_Id := Namet.No_Name;
      --  The supported language, always lower case

      Selectable    : Boolean := True;
      Selected      : Boolean := False;
      Complete      : Boolean := True;
      Index_In_List : Character := ASCII.NUL;
   end record;
   No_Compiler : constant Compiler;
   --  Describes one of the compilers found on the PATH.
   --  Path is the directory that contains the compiler executable.
   --  Path_Order is used for sorting in the interactive menu: it indicates the
   --  index in $PATH of the directory, so that we can show first the compilers
   --  that are first in path.
   --  Any of these compilers can be selected by the user as part of a config.
   --  However, to prevent incompatibilities, a compiler can be marked as not
   --  selectable. This will be re-evaluated based on the current selection.
   --  Complete is set to True if all the information about the compiler was
   --  computed. It is set to False if the compiler was specified through a
   --  command line argument --config, and part of the info needs to be
   --  computed.
   --  Index_In_List is used for the interactive menu, and is initialized
   --  automatically.

   package Compiler_Lists is new Ada.Containers.Doubly_Linked_Lists (Compiler);
   --  A list of compilers.

   type Compiler_Iterator is abstract tagged null record;
   --  An iterator for searches for all known compilers in a list of
   --  directories. Whenever a new compiler is found, the Callback primitive
   --  operation is called.

   procedure Callback
     (Iterator       : in out Compiler_Iterator;
      Base           : in out Knowledge_Base;
      Comp           : Compiler;
      From_Extra_Dir : Boolean;
      Continue       : out Boolean) is abstract;
   --  Called whenever a new compiler is discovered.
   --  It might be discovered either in a path added through a --config
   --  parameter (in which case From_Extra_Dir is True), or in a path specified
   --  in the environment variable $PATH (in which case it is False). If the
   --  directory is both in Extra_Dirs and in $PATH, From_Extra_Dir is set to
   --  False.
   --  On exit, Continue should be set to False if there is no need to discover
   --  further compilers (however there will be no possibility to restart the
   --  search at the same point later on).

   procedure Foreach_Compiler_In_Path
     (Iterator            : in out Compiler_Iterator;
      Base                : in out Knowledge_Base;
      On_Target           : Targets_Set_Id;
      Extra_Dirs          : String := "");
   --  Find all compilers in "Extra_Dirs & $PATH".
   --  Extra_Dirs should typically be the list of directories found in
   --  --config command line arguments.
   --  The only filtering done is the target, for optimization purposes (no
   --  need to computed all info about the compiler if we know it will not be
   --  uses anyway).

   procedure Known_Compiler_Names
     (Base : Knowledge_Base;
      List : out Ada.Strings.Unbounded.Unbounded_String);
   --  Set List to the comma-separated list of known compilers

   function To_String
     (Comp          : Compiler;
      As_Config_Arg : Boolean;
      Show_Target   : Boolean := False) return String;
   --  Return a string representing the compiler. It is either the --config
   --  argument (if As_Config_Arg is true) or the string to use in the
   --  interactive menu otherwise.

   function To_String
     (Compilers     : Compiler_Lists.List;
      Selected_Only : Boolean;
      Show_Target   : Boolean := False) return String;
   --  Return the list of compilers.
   --  Unselectable compilers are hidden. If Selected_Only is true, then only
   --  compilers that are currently selected are displayed.

   procedure Generate_Configuration
     (Base        : Knowledge_Base;
      Compilers   : Compiler_Lists.List;
      Output_File : String);
   --  Generate the configuration file for the list of selected compilers

   function Is_Supported_Config
     (Base      : Knowledge_Base;
      Compilers : Compiler_Lists.List) return Boolean;
   --  Whether we know how to link code compiled with all the selected
   --  compilers.

   function Is_Language_With_No_Compiler
     (Base        : Knowledge_Base;
      Language_LC : String) return Boolean;
   --  Given a language name (lower case), returns True if that language is
   --  known to require no compiler

   procedure Get_Targets_Set
     (Base   : in out Knowledge_Base;
      Target : String;
      Id     : out Targets_Set_Id);
   --  Get the target alias set id for a target.  If not already in the base,
   --  add it.

   function TU (Str : String) return Ada.Strings.Unbounded.Unbounded_String;
   --  returns an unbounded string for Str (or Null_Unbounded_String if
   --  Str is the empty string)

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   procedure Get_Words
     (Words  : String;
      Filter : Namet.Name_Id;
      Separator1 : Character;
      Separator2 : Character;
      Map    : out String_Lists.List;
      Allow_Empty_Elements : Boolean);
   --  Return the list of words in Words. Splitting is done on special
   --  characters, so as to be compatible with a list of languages or a list of
   --  runtimes
   --  If Allow_Empty_Elements is false, then empty strings are not stored in
   --  the list.

   function Get_Program_Directory return String;
   --  Get the directory in which the application is installed. For instance,
   --  it would return /usr/local if the gprconfig executable is
   --  /usr/local/bin/gprconfig.
   --  The returned value always ends with a directory separator

   function Name_As_Directory (Dir : String) return String;
   --  Ensure that Dir ends with a directory separator

   procedure Put_Verbose (Str : String; Indent_Delta : Integer := 0);
   --  Print Str if verbose mode is activated.
   --  Indent_Delta will increase the current indentation level for all further
   --  traces, which is used to highlight nested calls. Only the sign of
   --  Indent_Delta is taken into account.
   --  Nothing is printed if Str is the empty string, only the indentation is
   --  changed

   function Get_String (Str : String) return Namet.Name_Id;
   function Get_String_Or_No_Name (Str : String) return Namet.Name_Id;
   --  Same as Name_Find, but does not require the user to modify
   --  Name_Buffer manually.
   --  The second version returns No_Name is the string is empty

   type Compare_Type is (Before, Equal, After);
   function Compare (Name1, Name2 : Namet.Name_Id) return Compare_Type;
   --  Compare alphabetically two strings

private
   No_Compiler : constant Compiler :=
     (Name        => Namet.No_Name,
      Target      => Namet.No_Name,
      Targets_Set => Unknown_Targets_Set,
      Executable  => Namet.No_Name,
      Base_Name   => Namet.No_Name,
      Path        => Namet.No_Name,
      Variables   => Variables_Maps.Empty_Map,
      Version     => Namet.No_Name,
      Prefix      => Namet.No_Name,
      Runtime     => Namet.No_Name,
      Runtime_Dir => Namet.No_Name,
      Language_Case => Namet.No_Name,
      Language_LC => Namet.No_Name,
      Selectable  => False,
      Selected    => False,
      Complete    => True,
      Index_In_List => ASCII.NUL,
      Path_Order  => 0);

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;

   type External_Value_Type is (Value_Constant,
                                Value_Shell,
                                Value_Directory,
                                Value_Grep,
                                Value_Filter,
                                Value_Must_Match,
                                Value_Variable,
                                Value_Done);
   type External_Value_Node
     (Typ : External_Value_Type := Value_Constant) is
      record
         case Typ is
            when Value_Constant  =>
               Value           : Namet.Name_Id;
            when Value_Shell     =>
               Command         : Namet.Name_Id;
            when Value_Directory  =>
               Directory       : Namet.Name_Id;
               Directory_Group : Integer;
               Dir_If_Match    : Namet.Name_Id;
            when Value_Grep       =>
               Regexp_Re       : Pattern_Matcher_Access;
               Group           : Natural;
            when Value_Filter     =>
               Filter          : Namet.Name_Id;
            when Value_Must_Match =>
               Must_Match      : Namet.Name_Id;
            when Value_Variable =>
               Var_Name        : Namet.Name_Id;
            when Value_Done =>
               null;
         end case;
      end record;

   package External_Value_Nodes is new Ada.Containers.Doubly_Linked_Lists
     (External_Value_Node);

   subtype External_Value is External_Value_Nodes.List;

   Null_External_Value : constant External_Value :=
     External_Value_Nodes.Empty_List;

   type Compiler_Description is record
      Name             : Namet.Name_Id := Namet.No_Name;
      Executable       : Namet.Name_Id := Namet.No_Name;
      Executable_Re    : Pattern_Matcher_Access;
      Prefix_Index     : Integer := -1;
      Target           : External_Value;
      Version          : External_Value;
      Variables        : External_Value;
      Languages        : External_Value;
      Runtimes         : External_Value;
      Default_Runtimes : String_Lists.List;
   end record;
   --  Executable_Re is only set if the name of the <executable> must be
   --  taken as a regular expression.

   package Compiler_Description_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Namet.Name_Id, Compiler_Description,
        Hash_Case_Insensitive, Namet."=");

   type Compiler_Filter is record
      Name        : Namet.Name_Id;
      Version     : Namet.Name_Id;
      Version_Re  : Pattern_Matcher_Access;
      Runtime     : Namet.Name_Id;
      Runtime_Re  : Pattern_Matcher_Access;
      Language_LC : Namet.Name_Id;
   end record;
   --  Representation for a <compiler> node (in <configuration>)

   package Compiler_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compiler_Filter);

   type Compilers_Filter is record
      Compiler : Compiler_Filter_Lists.List;
      Negate   : Boolean := False;
   end record;
   No_Compilers_Filter : constant Compilers_Filter :=
     (Compiler => Compiler_Filter_Lists.Empty_List,
      Negate   => False);
   --  a <compilers> filter, that matches if any of its <compiler> child
   --  matches.

   package Compilers_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compilers_Filter);

   type Configuration is record
      Compilers_Filters : Compilers_Filter_Lists.List;
      Targets_Filters   : String_Lists.List;  --  these are regexps
      Negate_Targets    : Boolean  := False;
      Config            : Namet.Name_Id;

      Supported         : Boolean;
      --  Whether the combination of compilers is supported
   end record;

   package Configuration_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Configuration);

   package Target_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Pattern_Matcher_Access);

   package Targets_Set_Vectors is new Ada.Containers.Vectors
     (Known_Targets_Set_Id, Target_Lists.List, Target_Lists."=");

   type Knowledge_Base is record
      Compilers               : Compiler_Description_Maps.Map;
      No_Compilers            : String_Lists.List;
      Check_Executable_Regexp : Boolean := False;
      Configurations          : Configuration_Lists.List;
      Targets_Sets            : Targets_Set_Vectors.Vector;
   end record;
   --  Check_Executable_Regexp is set to True if at least some of the
   --  executable names are specified as regular expressions. In such a case,
   --  a slightly slower algorithm is used to search for compilers.
   --  No_Compilers is the list of languages that require no compiler, and thus
   --  should not be searched on the PATH.

end GprConfig.Knowledge;
