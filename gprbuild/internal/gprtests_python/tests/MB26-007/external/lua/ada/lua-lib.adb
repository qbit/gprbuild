-- Lua standard libraries

package body Lua.Lib is

   function New_Metatable (State : Lua.State_t; Name : String) return Boolean is
   begin
      Lua.Push_String (State, Name);
      Lua.Raw_Get (State, Lua.Registry_Index);
      if Lua.Is_Nil (State, -1) = False then
         return False;
      end if;
      Lua.Pop (State, 1);
      Lua.New_Table (State);
      -- 1 metatable
      Lua.Push_String (State, Name);
      -- 1 metatable
      -- 2 Name
      Lua.Push_Value (State, -2);
      -- 1 metatable
      -- 2 Name
      -- 3 metatable
      Lua.Raw_Set (State, Lua.Registry_Index);
      -- 1 metatable
      Lua.Push_Value (State, -1);
      -- 1 metatable
      -- 2 metatable
      Lua.Push_String (State, Name);
      -- 1 metatable
      -- 2 metatable
      -- 3 Name
      Lua.Raw_Set (State, Lua.Registry_Index);
      -- 1 metatable
      return True;
   end New_Metatable;

   procedure Open_Library
     (State     : Lua.State_t;
      Name      : String;
      Functions : Register_Array_t;
      num_upvalues  : Integer)
   is
   begin

      if Name /= "" then
         -- we check whether the name already exists
         Lua.Get_Global(State, Name );
         -- if it doesn't, we create it
         if Lua.Is_Nil (State, -1) then
            -- remove the nil value that get_global returned
            Lua.Pop (State, 1);
            Lua.New_Table (State);
            -- we duplicate the table
            Lua.Push_Value( State, -1 );
            --  __G[ Name ] = table
            --  and pops the table
            Lua.Set_Global(State, Name );
         end if;
         -- ??
         Lua.Insert (State, -(num_upvalues + 1));
      end if;

      -- then we can happily push our functions, along with their
      -- upvalues
      for Index in Functions'First .. Functions'Last loop
         Lua.Push_String (State, Functions (Index).Name);
         for j in 1 .. num_upvalues loop
            Lua.Push_Value (State, -(num_upvalues + 1));
         end loop;
         Lua.Push_User_Closure (State, Functions (Index).Func, num_upvalues);
         Lua.Set_Table (State, -(num_upvalues + 3));
      end loop;

      Lua.Pop (State, num_upvalues);
   end Open_Library;

end Lua.Lib;
