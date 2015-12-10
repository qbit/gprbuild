
With FCAPI;

Package Body FC_Manager is

   Function FibreLock (HostID : in FCAPI.U8) return integer;
   Pragma Export (C, FibreLock, "FibreLockAda");
   
   Function FibreLock (HostID : in FCAPI.U8) return integer is
   Begin
      return 1;            
   End FibreLock;
                                                     
   -- --------------------------------------------------------------------- --
   -- --------------------------------------------------------------------- --
                                                               
   Function FibreRelease (HostID : in FCAPI.U8) return integer;
   Pragma Export (C, FibreRelease, "FibreReleaseAda");

   
   Function FibreRelease (HostID : in FCAPI.U8) return integer is
   Begin
      return 1;            
   End FibreRelease;


   Procedure Dummy is
   Begin
      null;
   End Dummy;


Begin      
   null;
End FC_Manager;

