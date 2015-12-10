
with GNAT.Semaphores;
package SEMAPHORES is

   type SEMAPHORE is private;
   Semaphore_Exception : exception;

   -- Subtype BINARY_VALUE defines the range of allowable values for binary
   -- semaphores.

   subtype BINARY_VALUE is INTEGER range 0..1;


   -- The function BINARY_SEMAPHORE creates a binary semaphore
   -- with an initial value denoted by the parameter INITIAL_VALUE.
   -- If the value of parameter CHECKED is set to false, no exception will
   -- be raised if a signal operation is carried out when the value of the
   -- semaphore is 1.

   function BINARY_SEMAPHORE   (INITIAL_VALUE  : in     BINARY_VALUE;
                                CHECKED        : in     BOOLEAN := TRUE) return
                                                                      SEMAPHORE;

   -- The procedure SIGNAL provides the functionality of the signal operation.

   procedure SIGNAL            (S        : in out SEMAPHORE);


   -- The procedure INT_SIGNAL is functionally identical to the corresponding
   -- SIGNAL procedure except that it is designed to be called from within
   -- interrupt handlers. This routine may not be called for SEMAPHORE objects
   -- with a CHECKED attribute of TRUE. SEMAPHORE_ERROR will be raised if
   -- this rule is violated.

   procedure INT_SIGNAL        (S        : in out SEMAPHORE);


   -- The procedure WAIT provides the functionality of the wait operation.

   procedure WAIT              (S        : in out SEMAPHORE);


   -- The procedure TIMED_WAIT performs the wait operation, and sets the
   -- parameter SUCCESS to true if the task can proceed within the time period
   -- denoted by the parameter TIMEOUT.

   procedure TIMED_WAIT        (S        : in out SEMAPHORE;
                                TIMEOUT  : in     DURATION;
                                SUCCESS  : out    BOOLEAN);


   -- The procedure CONDITIONAL_WAIT performs the wait operation, and sets the
   -- parameter SUCCESS to true if the task can proceed immediately.

   procedure CONDITIONAL_WAIT  (S        : in out SEMAPHORE;
                                SUCCESS  : out    BOOLEAN);


private

   type SEMAPHORE_TYPE is access GNAT.Semaphores.Binary_Semaphore;

   type SEMAPHORE_RECORD is record
      Checked : BOOLEAN;
      Flag : Semaphore_Type;
   end record;

   type SEMAPHORE is access SEMAPHORE_RECORD;



end SEMAPHORES;
