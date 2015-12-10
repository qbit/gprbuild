with Ada.Text_IO; use Ada.Text_IO;

package body Lua.AuxLib is

   -- number of arguments
   procedure Check_N_Arguments (L : Lua.State_t; N_Arguments : Integer) is
      N : constant Integer := Lua.Get_Top (L);
   begin
      if N /= N_Arguments then
         Lua.Push_String
           (L,
            "Invalid number of arguments, should be " & Integer'Image (N_Arguments));
         Lua.Error (L);
      end if;
   end Check_N_Arguments;

   package body Check_Argument_Type is

      procedure Check (L : Lua.State_t; Index : Integer) is
      begin
         if not Is_A (L, Index) then
            Lua.Push_String
              (L,
               "Incorrect argument type" &
               Integer'Image (Index) &
               " , " &
               " expected " &
               Lua.Type_t'Image (Lua_Type));
            Lua.Error (L);
         end if;
      end Check;

   end Check_Argument_Type;

   package Check_Number_Pkg is new Check_Argument_Type (Lua_Type => T_Number, Is_A => Is_Number);
   procedure Check_Number (L : Lua.State_t; Index : Integer) renames Check_Number_Pkg.Check;

   package Check_Boolean_Pkg is new Check_Argument_Type (
      Lua_Type => T_Boolean,
      Is_A     => Is_Boolean);
   procedure Check_Boolean (L : Lua.State_t; Index : Integer) renames Check_Boolean_Pkg.Check;

   package Check_String_Pkg is new Check_Argument_Type (Lua_Type => T_String, Is_A => Is_String);
   procedure Check_String (L : Lua.State_t; Index : Integer) renames Check_String_Pkg.Check;

   package Check_Table_Pkg is new Check_Argument_Type (Lua_Type => T_Table, Is_A => Is_Table);
   procedure Check_Table (L : Lua.State_t; Index : Integer) renames Check_Table_Pkg.Check;

   procedure Dump_Stack (L : Lua.State_t) is
      N : constant Integer := Lua.Get_Top (L);
   begin
      Put_Line ("Stack dump start, " & Integer'Image (N));
      Put_Line ("Stack size, " & Integer'Image (N));
      for I in reverse 1 .. N loop
         declare
            T : Type_t := Type_Of (L, I);
         begin
            Put (Positive'Image (I) & " => ");
            case T is
            when T_Number =>
               Put_Line ("Number ," & Number_t'Image (To_Number (L, I)));

            when T_String =>
               Put_Line ("String," & To_String (L, I));
            when T_Table =>
               Put("Table, ");
               -- this might actually be a light user data.
               -- we push the table on top of the stack
               Push_Value(L, I );
               -- and retrieve table.__kind
               Push_String(L, "__kind");
               Get_Table(L, Top_Of_Stack - 1 );
               -- if the result is a string, we display it
               if Is_String(L, Top_Of_Stack ) then
                  Put_Line ( To_String( L, Top_Of_Stack ) );
               end if;
               -- we pop the table and result
               Pop( L, 2 );
            when T_None =>
               Put_Line ("None");
            when T_Nil =>
               Put_Line ("Nil");
            when T_Function =>
               Put_Line ("Function");
            when T_Userdata =>
               Put_Line ("Userdata");
            when T_Light_Userdata =>
               Put_Line ("Lightuserdata");
            when T_Thread =>
               Put_Line ("Thread");
            when T_Boolean =>
               Put_Line ("Boolean," & Boolean'Image (To_Boolean (L, I)));
            end case;
         end;
      end loop;
      Put_Line ("Stack dump end, " & Integer'Image (N));
   end Dump_Stack;

   procedure Get_Array_Value (L : Lua.State_t; I : Integer) is
   begin
      Check_Table (L, Top_Of_Stack);

      Push_Number (L, Number_t (I));
      Get_Table (L, Top_Of_Stack - 1);
   end Get_Array_Value;

   function Get_Array_Value (L : Lua.State_t; I : Integer) return Number_t is
      Result : Number_t;
   begin
      Get_Array_Value (L, I);
      Check_Number (L, Top_Of_Stack);
      Result := To_Number (L, Top_Of_Stack);
      Pop (L, 1);
      return Result;
   end Get_Array_Value;

   procedure Get_Table_Field (L : Lua.State_t; Field : String) is
   begin
      Check_Table (L, Top_Of_Stack);
      Push_String (L, Field);
      Get_Table (L, Top_Of_Stack - 1);
      -- just to be cautious
      if Is_Nil (L, Top_Of_Stack) then
         raise Lua_Exception with "Could not find field '" & Field & "' in table ";
      end if;
   end Get_Table_Field;

   procedure Error (L : Lua.State_t) is
   begin
      raise Lua_Exception with To_String (L, Top_Of_Stack);
   end Error;
   pragma No_Return (Error);

   -- various exec routines, with exception-based error handling.
   procedure Exec_File (L : Lua.State_t; Filename : String) is
      E : Lua.Error_t;
   begin
      E := Lua.Exec_File (L, Filename);
      if E /= Lua_Error_None then
         Error (L);
      end if;
   end Exec_File;

   procedure Exec_String (L : Lua.State_t; Statement : String
                          ; Buffer_Name : in String) is
      E : Lua.Error_t;
   begin
      E := Lua.Exec_String (L, Statement, Buffer_Name);
      if E /= Lua_Error_None then
         Error (L);
      end if;
   end Exec_String;


--     function Traceback( L : Lua.State_T ) return Lua.Integer_t;
--     pragma Convention(C, Traceback );
--     function Traceback( L : Lua.State_T ) return Lua.Integer_t is
--     begin
--          if not Is_String(L, 1 ) then
--             return 1;
--          end if;
--
--          Get_Global(L, "debug");
--          if not Is_Table(L, Top_Of_Stack ) then
--             Pop(L, 1 );
--             return 1;
--          end if;
--
--          Get_field(L, Top_Of_Stack, "traceback" );
--          if not Is_Function(L, Top_Of_Stack ) then
--             Pop(L, 2 );
--              return 1;
--          end if;
--
--          Push_Value(L, 1 );
--          Push_Integer(L, 2 );
--          Call(L, 2, 1 );
--          return 1;
--     end Traceback;


   procedure Protected_Call (L : Lua.State_t; Num_Arguments : Integer; Num_Results : Integer) is
--        E               : Lua.Error_t;
--        Bottom_Of_Stack : constant := 1;
   begin
      -- we move the traceback function to the bottom of the stack
--        Push_User_Function( L, Traceback'access );
--        Insert( L, Bottom_Of_Stack );

      -- and then do our call
--        E := Lua.Protected_Call (L, Num_Arguments, Num_Results, Bottom_Of_Stack);
--        E := Lua.Call (L, Num_Arguments, Num_Results);
--        if E /= Lua_Error_None then
--           Error (L);
--        end if;
      Lua.Call (L, Num_Arguments, Num_Results);
   end Protected_Call;



end Lua.AuxLib;
