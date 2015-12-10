pragma Ada_05;

with System; use System;
with DDS.Condition; use DDS.Condition;
with DDS.Condition_Impl;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body DDS.WaitSet_Impl is

   procedure Get_Conditions_I
     (Self    : access Ref;
      CondSeq : access DDS.ConditionSeq.Sequence;
      Active  : DDS.Boolean);

   procedure Initialize
     (Self : in out Ref)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "DDS_WaitSet_new");
   begin
      Self.SetInterface (Internal);
   end Initialize;

   procedure Adjust
     (Self : in out Ref)
   is
   begin
      null;
   end Adjust;

   procedure Finalize
     (Self : in out Ref)
   is
      procedure Internal (CWaitSet : System.Address);
      pragma Import (C, Internal, "DDS_WaitSet_delete");
   begin
      Internal (Self.GetInterface);
   end Finalize;

   procedure Wait
     (Self              : not null access Ref;
      Active_Conditions : access DDS.ConditionSeq.Sequence;
      Timeout           : in DDS.Duration_T)
   is
      Retcode : DDS.ReturnCode_T;
      CWaitSet : System.Address;

      function Internal_Wait (CWaitSet : System.Address;
                              CondSeq  : access DDS.ConditionSeq.Sequence;
                              Timeout  : in DDS.Duration_T)
                             return DDS.ReturnCode_T;
      pragma Import (C, Internal_Wait, "DDS_WaitSet_waitI");

      function Internal_End_Wait (CWaitSet : System.Address)
                                 return DDS.ReturnCode_T;
      pragma Import (C, Internal_End_Wait, "DDS_WaitSet_end_waitI");
   begin
      CWaitSet := Self.GetInterface;
      Retcode := Internal_Wait (CWaitSet, Active_Conditions, Timeout);

      if (Retcode = DDS.RETCODE_OK) then
         begin
            Get_Conditions_I (Self, Active_Conditions, True);
         exception
            when others =>
               Retcode := Internal_End_Wait (CWaitSet);
               raise;
         end;
         Retcode := Internal_End_Wait (CWaitSet);
      end if;
   end Wait;

   procedure Attach_Condition
     (Self : not null access Ref;
      Cond : access DDS.Condition.Ref'Class)
   is
      Impl : access DDS.Condition_Impl.Ref'Class;

      function Internal
        (CWaitSet : System.Address;
         CCondition : System.Address)
        return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_WaitSet_attach_condition");

   begin
      if Cond = null then
         raise DDS.BAD_PARAMETER;
      end if;

      Impl := Cond.Get_Impl_I;
      Ret_Code_To_Exception (Internal (Self.GetInterface, Impl.GetInterface));
   end Attach_Condition;

   procedure Detach_Condition
     (Self : not null access Ref;
      Cond : access DDS.Condition.Ref'Class)
   is
      Impl : access DDS.Condition_Impl.Ref'Class;

      function Internal
        (CWaitSet : System.Address;
         CCondition : System.Address)
        return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_WaitSet_detach_condition");
   begin
      if Cond = null then
         raise DDS.BAD_PARAMETER;
      end if;

      Impl := Cond.Get_Impl_I;
      Ret_Code_To_Exception (Internal (Self.GetInterface, Impl.GetInterface));
   end Detach_Condition;

   procedure Get_Conditions
     (Self                : not null access Ref;
      Attached_Conditions : access DDS.ConditionSeq.Sequence)
   is
   begin
      Get_Conditions_I (Self, Attached_Conditions, False);
   end Get_Conditions;

   function Get_Impl_I
     (Self : access Ref)
     return access DDS.WaitSet_Impl.Ref
   is
   begin
      return Self;
   end Get_Impl_I;

   procedure Get_Conditions_I
     (Self    : access Ref;
      CondSeq : access DDS.ConditionSeq.Sequence;
      Active  : DDS.Boolean)
   is
      Max_Length, Length, New_Max : DDS.Long;
      Condition : DDS.Condition.Ref_Access;
      Owned : DDS.Boolean;
      CCondition : System.Address;
      CWaitSet : System.Address;

      function Start_Iterator
        (CWaitSet : System.Address;
         Active   : DDS.Boolean)
        return DDS.Long;
      pragma Import (C, Start_Iterator, "DDS_WaitSet_start_conditions_iteratorI");

      procedure End_Iterator
        (CWaitSet : System.Address);
      pragma Import (C, End_Iterator, "DDS_WaitSet_end_conditions_iteratorI");

      function Get_Next
        (CWaitSet : System.Address;
         Active   : DDS.Boolean)
        return System.Address;
      pragma Import (C, Get_Next, "DDS_WaitSet_get_next_conditionI");

      function Get_User_Object
        (CCondition : System.Address)
        return DDS.Condition.Ref_Access;
      pragma Import (C, Get_User_Object, "DDS_Condition_get_user_objectI");

   begin
      CWaitSet := Self.GetInterface;

      -- Check arguments
      Owned := DDS.ConditionSeq.Has_Ownership (CondSeq);
      Max_Length := DDS.ConditionSeq.Get_Maximum (CondSeq);

      New_Max := Start_Iterator (CWaitSet, Active);

      if (Owned and then New_Max > Max_Length) then
         begin
            DDS.ConditionSeq.Set_Maximum (CondSeq, New_Max);
            Max_Length := New_Max;
         exception
            when DDS.ConditionSeq.SEQUENCE_ERROR =>
               End_Iterator (CWaitSet);
               raise DDS.ERROR with "!maximum";
         end;
      end if;

      -- Loop to collect conditions
      Length := 0;
      CCondition := Get_Next (CWaitSet, Active);
      while (CCondition /= System.Null_Address) loop
         if Length < Max_Length then
            begin
               DDS.ConditionSeq.Set_Length (CondSeq, Length + 1);
               Condition := Get_User_Object (CCondition);

               Length := Length + 1;
               DDS.ConditionSeq.Set_Element (CondSeq, Length, Condition);

               CCondition := Get_Next (CWaitSet, Active);
            exception
               when Constraint_Error =>
                  End_Iterator (CWaitSet);
                  raise;
            end;
         elsif Owned then
            End_Iterator (CWaitSet);
            raise DDS.ERROR with "length inconsistent with max_length";
         else
            End_Iterator (CWaitSet);
            raise DDS.OUT_OF_RESOURCES with "sequence full";
         end if;
      end loop;
      End_Iterator (CWaitSet);
   end Get_Conditions_I;

end DDS.WaitSet_Impl;
