with GNAT.Semaphores;
with System;

package body SEMAPHORES is

   ----------------------
   -- BINARY_SEMAPHORE --
   ----------------------

   function BINARY_SEMAPHORE
     (INITIAL_VALUE  : in     BINARY_VALUE;
      CHECKED        : in     BOOLEAN := TRUE)
      return SEMAPHORE
   is

   begin

      return new Semaphore_RECORD'((CHECKED => Checked,
                            Flag => new GNAT.Semaphores.Binary_Semaphore(Initially_Available => Initial_Value = 1,
                                                  Ceiling => System.Default_Priority)));

   end BINARY_SEMAPHORE;

   ------------
   -- SIGNAL --
   ------------

   procedure SIGNAL (S        : in out SEMAPHORE) is
   begin
      if S.All.Checked then

         Select

            S.All.Flag.All.Seize;

            S.All.Flag.All.Release;

            raise Semaphore_Exception;

         else

            S.All.Flag.All.Release;


         end Select;
      else


         S.All.Flag.All.Release;
      end if;



      end SIGNAL;


   ----------------
   -- INT_SIGNAL --
   ----------------

   procedure INT_SIGNAL (S        : in out SEMAPHORE) is
   begin
      Signal(S);
   end INT_SIGNAL;

   ----------
   -- WAIT --
   ----------

   procedure WAIT (S        : in out SEMAPHORE) is
   begin
      S.All.Flag.All.Seize;
   end WAIT;

   ----------------
   -- TIMED_WAIT --
   ----------------

   procedure TIMED_WAIT
     (S        : in out SEMAPHORE;
      TIMEOUT  : in     DURATION;
      SUCCESS  : out    BOOLEAN)
   is
   begin
      Select
         S.All.Flag.All.Seize;
         SUCCESS := TRUE;
      or
         Delay TIMEOUT;
         SUCCESS := False;
      end Select;

   end TIMED_WAIT;

   ----------------------
   -- CONDITIONAL_WAIT --
   ----------------------

   procedure CONDITIONAL_WAIT
     (S        : in out SEMAPHORE;
      SUCCESS  : out    BOOLEAN)
   is
   begin
     Select
         S.All.Flag.All.Seize;
         SUCCESS := TRUE;
      else
         SUCCESS := False;
      end Select;
   end CONDITIONAL_WAIT;

end SEMAPHORES;
