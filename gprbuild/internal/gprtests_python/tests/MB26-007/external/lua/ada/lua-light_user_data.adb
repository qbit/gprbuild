-- $Id

with ADA.Containers.Hashed_Maps;
with ADA.Containers.Doubly_Linked_Lists;
with ADA.Unchecked_Conversion; 
with ADA.Unchecked_Deallocation;
with ADA.Tags; use ADA.Tags;

with System.Address_To_Access_Conversions;

with Lua.AuxLib; use Lua.AuxLib;

package body Lua.Light_User_Data is

   package Convert is new System.Address_To_Access_Conversions (User_Data_t);
   use type System.Address;

   -- we need to maintain a map State_T => List of allocated of objects
   -- to provide some kind of extremely crude garbage collection.
   function Hash( State : Lua.State_t ) return Ada.Containers.Hash_Type 
   is
      -- System.Address ( the actual type behind Lua.State_t ) is private, and
      -- cannot be converted to integer.  So we define an identical type (
      -- Local_Address ), and use ADa.Unchecked_Conversion.
      type Local_Address is mod System.Memory_Size;
      function Convert is new Ada.Unchecked_Conversion( Source => Lua.State_t,
                                                       Target => Local_Address );
   begin
        return Ada.Containers.Hash_Type'Mod( Convert(State ) );
   end Hash;

   package Address_List is new ADA.Containers.Doubly_Linked_Lists (
                                Element_type => Light_User_Data_Access_t,
                                "="          => "=" );

   package State_To_Address_List is new ADA.Containers.Hashed_Maps(
            Key_Type     => Lua.State_t,
            Element_Type => Address_List.List,
            Hash         => Hash,
            Equivalent_Keys => "=",
            "="             => Address_List."=");

   -- we store the said map  here
   Allocated : State_To_Address_List.Map := State_To_Address_List.Empty_Map;
   -- We will store here the list of pointers which have been pushed
   -- This will allow us to do some limited kind of type checking
   Pushed : State_To_Address_List.Map := State_To_Address_List.Empty_Map;

   -- this is a hack to retrieve the name of the type we want to push 
   type Wrapper is tagged record
      T : Light_User_Data_Access_t;
   end record;
   TypeName : constant String := Expanded_Name( Wrapper'Class'Tag );


   Pointer_Field : constant String := "__pointer";
   Kind_Field    : constant String := "__kind";

   -- the actual C bindings to the lua library

   package C_Bindings is
      -- returns a void *, hence the System.Address
      function To_Light_User_Data
        (State : Lua.State_t;
         Index : Lua.Integer_t)
         return  System.Address;
      pragma Import (C, To_Light_User_Data, "lua_touserdata");

      procedure Push_Light_User_Data (State : Lua.State_t; Addr : System.Address);
      pragma Import (C, Push_Light_User_Data, "lua_pushlightuserdata");

   end C_Bindings;

   procedure Push (State : Lua.State_t; UD : Light_User_Data_Access_t) is
      use State_To_Address_List;
      use Address_List;
   begin
      if not Contains( Pushed, State ) then
         Include( Pushed, State, Empty_List );
      end if;

      -- we make sure that we keep track of what's been pushed, so that we can
      -- check that when we get the pointer, it's actually a legal one.
      Append( Pushed( State ), UD );

      -- because we are paranoid, we build a table containing two fields, the
      -- light user data and the typename.  We will then be able to match this
      -- on the way back.
      -- The table looks like { __pointer = (ada access value ), __kind = typename }
      New_Table( State );

      -- __pointer
      C_Bindings.Push_Light_User_Data
        (State => State,
         Addr  => Convert.To_Address (Convert.Object_Pointer ( UD )));
      Set_Field( State, Top_Of_Stack - 1, Pointer_Field );

      -- __kind
      Push_String( State, Typename );
      Set_Field( State, Top_Of_Stack - 1, Kind_Field );
   end Push;

   procedure Push (State : Lua.State_t; UD : User_Data_t) is
      UD_Access : Light_User_Data_Access_T := new User_Data_t'( UD );
      use State_To_Address_List;
      use Address_List;
   begin
      -- we store the references we allocate
      if not Contains( Allocated, State ) then
         Include( Allocated, State, Empty_List );
      end if;

      Append( Allocated( State ), UD_Access );

      -- once this is done, we can push the access type
      Push( State, UD_Access );
   end Push;

   -- releases all memory allocated by the objects that have been pushed
   procedure Cleanup( State : Lua.State_t ) is
      procedure Free is new Ada.Unchecked_Deallocation( User_Data_t, Light_User_Data_Access_T );
      Allocated_Objects  : Address_List.List := Allocated( State );
   begin
      -- we release all objects
      for Address of Allocated_Objects loop
        Free( Address );
      end loop;

      -- we empty the list
      Allocated( State ) := Address_List.Empty_List;
      Pushed( State )    := Address_List.Empty_List;
   end Cleanup;


   package Check_Light_User_Data is new Check_Argument_Type (
      Lua_Type => T_Userdata,
      Is_A     => Is_Light_Userdata);

   procedure Check (State : Lua.State_t; Index : Integer := 1) 
   is
   begin
      if not Is_Table( State, Index ) then
         raise Lua.Auxlib.Lua_Exception with "Not a light userdata of kind " 
                                            & typename 
                                            & " at index " 
                                            & Integer'Image( Index ) ;
      end if;

      -- we push the table at the top of the stack
      Push_Value( State, Index );
      Get_Table_Field( State, Kind_Field );

      declare
        Kind : constant String := To_String( State, Top_Of_Stack );
      begin
         if  Kind /= TypeName then
            raise Lua.Auxlib.Lua_Exception with "Light userdata has wrong kind : got " 
                                                    & Kind 
                                                    & " instead of " 
                                                    & Typename ;
         end if;
      end;
      Pop( State, 1 );

   end Check;

   function Get (State : Lua.State_t; Index : Integer := 1) return Light_User_Data_Access_T is

      use State_To_Address_List;
      use Address_List;
   begin
      -- Basic lua checks : we check that it's a table with the right fields,
      -- and push the table on top of the stack
      Check( State, Index );

      -- we retrieve the pointer field, and put it on top of the stack
      -- The stack will look like:
      --  top   : lightuserdata
      --  top-1 : table wrapper
      Get_Table_Field( State, Pointer_Field );
      Check_Light_User_Data.Check( State, Top_Of_Stack);

      -- once we are convinced that we have a proper light user data,
      -- we can actually retrieve the content
      declare
         Address : constant System.Address :=
           C_Bindings.To_Light_User_Data (State, Lua.Integer_t (Top_Of_Stack));
         Result : constant Light_User_Data_Access_t := 
           Light_User_Data_Access_T (Convert.To_Pointer (Address));
      begin
         -- we remove the light user data we've just retrieved
         -- along with the table wrapper which we no longer need
         Pop( State, 2 );

         -- one last sanity check : is this pointer actually valid ?
         if not Contains( Pushed( State ), Result ) then
           raise Lua.Auxlib.Lua_Exception with "Bad light userdata pointer at index " 
                                             & Integer'Image( Top_Of_stack ) 
                                             & ", expected pointer to " & Typename;
         end if;

         -- phew ! we're done.
         return Result;
      end;
   end Get;

end Lua.Light_User_Data;
